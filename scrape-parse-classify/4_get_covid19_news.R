#!/usr/local/bin/Rscript
######################################################################################

# file: get_covid19_news.R

# date: 01/04/2020

# Purpose: classify whether a news article is about covid19 if yes export and push it to github

######################################################################################
## Conditional instalation of the packages
packs <- c("tidyverse", "gdeltr2", "mailR", "reticulate", "caret", "text2vec")
for (pack in packs) {
  
  if (!requireNamespace(pack, quietly = TRUE)) {
    
    install.packages(pack)
    
  }
}
## load the relevant packages
require(tidyverse)

## source relevant files
# config python exe
reticulate::use_condaenv("anaconda3")
# source function
Sys.getenv("RETICULATE_PYTHON")
reticulate::source_python("scrape-parse-classify/2_news_parse.py")
source("scrape-parse-classify/1_gdelt_pull.R")
source("scrape-parse-classify/3_classify&push.R")

if (!dir.exists("scrape-parse-classify/logs")) {
  
  dir.create("scrape-parse-classify/logs")
  
}

cat(paste0("\n\n--- ", Sys.time(), " ---\n"),
    file = "scrape-parse-classify/logs/newsplease-parser.log",
    append = TRUE)

## 1 pull GDELT news metadata
##---------------------------------------------------------------------------

# set the time_range parameter depending on time of day
cur_time_range <- dplyr::case_when(
  as.numeric(format(Sys.time(), "%H")) %in% seq(0, 6) ~ "180 minutes",
  as.numeric(format(Sys.time(), "%H")) %in% seq(7, 20) ~ "40 minutes",
  as.numeric(format(Sys.time(), "%H")) %in% seq(20, 23) ~ "60 minutes"
)

# ## pull the metadata
gdelt_meta <- gdelt_call(time_range = cur_time_range) %>%
  filter(!is.na(urlArticle))

## 2 Parse the news
##-----------------------------------------------------------------------------
parsed_news <- purrr::map2(gdelt_meta$urlArticle, gdelt_meta$titleArticle, function(news_url, news_title) {
  
  ## parse
  parsed <- tryCatch(as_tibble(parse_news(news_url)),
                     error = function(e) NULL)
  
  print(parsed)
  
  if (is.data.frame(parsed) == FALSE || nrow(parsed) == 0 || nchar(parsed$maintext) < 10) {
    
    cat(paste0("\nARTICLE:\n", "\n -", news_title, " (", news_url, ")\nNot Parsed!\n\n"),
        file = "scrape-parse-classify/logs/newsplease-parser.log",
        append = TRUE)

  } else {
    
    to_return <- parsed %>%
      mutate(gdelt_article_title = news_title) %>%
      mutate_if(is.list, unlist) %>%
      mutate_all(list(~as.character(.))) %>%
      mutate_all(list(~na_if(.,"")))
    
    return(to_return)
    
  }
  
}) %>%
  bind_rows() %>%
  left_join(., gdelt_meta)

## 3 - Classify
##-------------------------------------------------------------------------
## unit of analysis: title + description. If the latter is missing, we pass resort to "maintext"
classifier_input <- parsed_news %>%
  mutate(pred_input = maintext) %>%
  distinct(url, .keep_all = TRUE)

# get the prediction
pred_data <- map2_df(classifier_input$pred_input, classifier_input$url, 
                     ~ (covid_classifier(.x) %>%  mutate(url = .y))) %>%
  mutate(about_covid = ifelse(about_covid == TRUE,
                            "1",
                            "0"))

# join. Keep covid related articles.
classified <- left_join(classifier_input, pred_data) %>%
  select(prediction_covid_topic = about_covid, title, description, everything()) %>%
  filter(prediction_covid_topic == "1")

if (nrow(classified) > 0) {

## 4 - Export and push
filename <- paste("data/", "covidpred_pt_",format(Sys.time(), "%Y-%M-%d_%H-%M"), ".csv")
# export
write_csv(classified,
          path = filename)
# push
automated_push(filename = filename)


}
