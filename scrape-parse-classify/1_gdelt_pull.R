#!/usr/local/bin/Rscript
######################################################################################

# file: 1_gdelt_pull.R

# date: 01/04/2020

# Purpose: API calls for all urls from portuguese domains

######################################################################################
### Setting things up
#-----------------------------------------------------------------------------------
cat("\n>> Setting things up\n")

## Conditional instalation of the packages
packs <- c("tidyverse", "gdeltr2", "mailR", "rvest")
for (pack in packs) {
  
  if (!requireNamespace(pack, quietly = TRUE)) {
    
    install.packages(pack)
    
  }
}

## Load
library(tidyverse)
library(gdeltr2)
library(git2r)
library(mailR)
library(rvest)

## Directories
# set working directory 

# move to scrape-parse-classify
setwd(gdelt_parent_dir)

# sub-dirs
if (!dir.exists("temp_data")){
  
  dir.create("temp_data")

}

if (!dir.exists("scripts")){
  
  dir.create("scripts")
  
}

#------------------------------------------------------------------------------
### API helper functions
#-----------------------------------------------------------------------------------
## 

## Notify that somehting went wrong
notifier <- function(er_dta = NULL) {
  
  mailR::send.mail(
    from = my_email, to = my_email, 
    subject = paste0("GDELT DATA: error at ", Sys.time()),
    body = paste0("At ", Sys.time(), " the code stopped working.\n", "Error message:\n", er_dta[1]),
    smtp = list(host.name = "smtp.gmail.com",
                port = 587,
                user.name = my_email,
                passwd =  readLines("/home/jmr/gmail_key.txt"),
                ssl = TRUE),
    authenticate = T,
    send = T)
  
}

### API call and raw GDELT data extraction
gdelt_call <- function(){
  
  ## Make query by theme. all medical classes plus 500 random classes
  covid_theme_gkg <- try(ft_v2_api(timespans = "40 minutes",
                                   gkg_themes = c(medical_themes$idGKGTheme, sample(non_medical_themes$idGKGTheme, 500)),
                                   source_countries = "PO",
                                   source_languages = "por",
                                   translate = FALSE,
                                   modes = "artlist",
                                   visualize_results = FALSE,
                                   return_message = TRUE) %>%
                           distinct(urlArticle, .keep_all = TRUE), silent = TRUE)
  
  ## Make a query by sentiment
  if (class(covid_theme_gkg)) {
    
    # notify via email
    notifier(er_dta = covid_theme_gkg)
    
  }
  
  cat(">> GDELT query --> done.\n")
  return(covid_theme_gkg)
  
}

### Scrape the text of the news
gdelt_scraper <- function(gdelt_tab = NULL) {
  
  stopifnot(any(class(gdelt_tab) == "tbl"))
  
  ## scrape in a loop using a mapping function
  article_content <- purrr::map_df(gdelt_tab$urlArticle, function(page) {
      
      (page <- sample(gdelt_tab$urlArticle, 1))
      
      ## parse
      parsed <- read_html(page)
      
      ### Selecting the text node
      ## method 1, rules-based
      parsed_txt <- parsed %>%
        html_nodes(xpath = '//*[@*[contains(., "art")]]//p') %>%
        html_text() %>%
        subset(., nchar(.) > 50) %>%
        paste(., collapse = "\n")
      
      if (nchar(parsed_txt) < 2) {
        
        ## method to, all nodes p with more than 6 characters
        parsed_txt <- parsed %>%
          html_nodes(xpath = '//p') %>%
          html_text() %>%
          subset(., nchar(.) > 50) %>%
          paste(., collapse = "\n")
        
      }
      
      print(parsed_txt)
      browseURL(page)
    
  })
  
  
  
}

