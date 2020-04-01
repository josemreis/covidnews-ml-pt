#!/usr/local/bin/Rscript
#!/usr/bin/Rscript
#!/usr/bin/env Rscript
### Setting things up
###------------------------------------------------------------------------------------------
## conditional install of packs
packs <- c("tidyverse", "caret", "jsonlite", "fastrtext", "text2vec")
for (pack in packs) {
  
  if (!requireNamespace(pack, quietly = TRUE)) {
    
    install.packages(pack)
    
  }
}
## Load the relevant packages
require(tidyverse)
require(caret)
require(jsonlite)
require(fastrtext)
require(text2vec)
try(theme_set(theme_minimal()), silent = TRUE) # set plot theme

## working dir
root_dir <- here::here()

setwd("train")

### Unit of analysis: title + leading_paragraph
###---------------------------------------------------------------------------------------
load_dta <- function(filename){
  
  ## parse json
  parsed <- fromJSON(filename) 
  
  # check if empty
  if (length(parsed) == 0) {
    
    cat(paste0("\n>> ", filename, " has no json.\n"), 
        file = "logs/json_parse.txt", 
        append = TRUE)
    
  } else {
    
    # turn into tibble and flatten all lists
    my_tbl <- parsed %>%
      as_tibble() %>%
      mutate_if(is.list, unlist)
    
    ## add missing labels
    if (!"has_corona_label" %in% names(my_tbl)) {
      
      my_tbl$has_corona_label <- ifelse(
        str_extract(filename, "(?<=\\_data\\/).*?(?=\\-)") == "no_covid_label",
        FALSE,
        TRUE
      )
      
    }
    
    ## rename a couple of vars
    to_return <- my_tbl %>%
      rename(is_covid = has_corona_label)
    
    
    return(to_return)
    
    
  }
  
}

## sub dir for the log files
if (!dir.exists("logs")) {
  
  dir.create("logs")
  
}

# sub-dir for data
if (!dir.exists("data")) {
  
  dir.create("data")
  
}

listed_files <- list.files("data/labeled_data/labeled_data", full.names = TRUE) %>%
  subset(., stringr::str_detect(., "json"))

# load
dta_raw <- map_df(listed_files, load_dta)

# export
write_csv(dta_raw, 
          "data/0_data_parsed.csv")

## prep
dta_raw <- readr::read_csv("data/0_data_parsed.csv") %>%
  mutate(lp_join = if_else(nchar(leading_paragraph) < 100,
                           remaining_content,
                           leading_paragraph),
         is_covid = ifelse(is_covid == TRUE,
                           "1", 
                           "0")) %>%
  unite(., col = "text", c("headlines", "lp_join"), sep = " ") %>%
  distinct(text, is_covid, .keep_all = TRUE)

### Models
###---------------------------------------------------------------------------------------
### partition 70, 30
set.seed(1234)
trainIndex <- createDataPartition(dta_raw$is_covid, p = 0.7, list = FALSE, times = 1)

### Random forests
## Prep the input text
prep_train <- function(txt) {
  
  prep_fun = tolower
  tok_fun = word_tokenizer
  
  it <- itoken(txt,
               preprocessor = prep_fun, 
               tokenizer = tok_fun,
               progressbar = TRUE)
  
  ## create vocabulary
  vocab <- create_vocabulary(it, ngram = c(1L, 5L), stopwords = tm::stopwords("pt"))
  
  vocab <- prune_vocabulary(vocab, term_count_min = 10, 
                            doc_proportion_max = 0.3)
  
  saveRDS(vocab, file = "final_model/rf-vectorizer.rds")
  
  ## vectorizer
  vectorizer <- vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- create_dtm(it, vectorizer)
  
  tfidf <- TfIdf$new()
  # fit model to train data and transform train data with fitted model
  dtm_text_tfidf <- fit_transform(dtm_text, tfidf)
  
  ## export tfidf model
  saveRDS(dtm_text_tfidf, file = "final_model/tfidf-model.rds")
  
  ## return
  return(list(dtm = dtm_text_tfidf, tfidf_model = tfidf))
  
}

## fit the model
# prep train data
train <- dta_raw[trainIndex,]
clean_train <- prep_train(train$text)
dtm <- clean_train$dtm %>%
  as.matrix() %>%
  as.data.frame()


# Train using the  best model
tgrid <- expand.grid(
  mtry = c(30, 40, 60),
  splitrule = "gini",
  min.node.size = c(20, 30, 40)
)

rf_mod <- train(x = as.data.frame(dtm), 
                y = as.factor(train[['is_covid']]), 
                method = "ranger",
                trControl = trainControl(method="repeatedcv",
                                         number = 10,
                                         repeats = 3, 
                                         verboseIter = T),
                tuneGrid = tgrid)

# prep test
prep_test <- function(txt, tfidf_model = clean_train$tfidf_model) {
  
  prep_fun = tolower
  tok_fun = word_tokenizer
  
  it <- itoken(txt,
               preprocessor = prep_fun, 
               tokenizer = tok_fun,
               progressbar = TRUE)
  
  ## vectorizer
  vocab <- readRDS("final_model/rf-vectorizer.rds")
  vectorizer <- vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- create_dtm(it, vectorizer)
  
  # # apply pre-trained tf-idf transformation to test data
  dtm_text_tfidf <- transform(dtm_text, tfidf_model)
  
  ## return
  return(dtm_text_tfidf)
  
}

test <- dta_raw[-trainIndex,]
dtm_test <- prep_test(test$text) %>%
  as.matrix() %>%
  as.data.frame()

# predict
predictions <- predict(rf_mod, newdata = dtm_test)

# confusion matrix
rf_cm <- confusionMatrix(predictions, as.factor(test$is_covid))
rf_cm

# save it
readr::write_rds(rf_mod,
                 path = "models/rf-model.rds")

## model-metrics-final
mod_metrics <- rbind(tibble(value = rf_cm$overall, metric = names(rf_cm$overall)),
                     tibble(value = rf_cm$byClass, metric = names(rf_cm$byClass))) %>%
  filter(metric %in% c("Accuracy", "Kappa", "F1", "Precision", "Recall")) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(model = "Random Forests\n('ranger' package, R 3.5.3.)\nWright MN, Ziegler A (2017). “ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R.” Journal of Statistical Software, 77(1), 1–17. doi: 10.18637/jss.v077.i01.",
         sample_size = rf_mod$finalModel$num.samples,
         train_prop_covid = sum(dta_raw$is_covid == "1")/nrow(dta_raw),
         mtry = rf_mod$finalModel$mtry,
         n_tree = rf_mod$finalModel$num.trees,
         min_node_size = rf_mod$finalModel$mtry,
         splitrule = "gini",
         model_type = "classification",
         fitted_on = Sys.Date()) %>%
  select(model_accuracy = Accuracy, model_kappa = Kappa, model_f1 = F1, model_precision = Precision, model_recall = Recall, everything())

png(filename = "final_model/rf-params.png", width = 800, height = 600)
plot(rf_mod)
dev.off()

readr::write_csv(mod_metrics,
                 path = "final_model/rf-model-metrics.csv")



