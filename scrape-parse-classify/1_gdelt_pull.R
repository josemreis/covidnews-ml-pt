#!/usr/local/bin/Rscript
######################################################################################

# file: 1_gdelt_pull.R

# date: 01/04/2020

# Purpose: API calls for all urls from portuguese domains

######################################################################################
### Setting things up
#-----------------------------------------------------------------------------------
## Load
library(tidyverse)
library(gdeltr2)
library(git2r)
library(mailR)
my_email <- readLines( '/home/jmr/my_gmail.txt')
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
gdelt_call <- function(time_range = "120 minutes"){
  
  ## Make query by theme. all medical classes plus 500 random classes
  gdelt_pt_all <- try(ft_v2_api(terms = "",
                                timespans = time_range,
                                source_countries = "PO",
                                source_languages = "por",
                                translate = FALSE,
                                modes = "artlist",
                                visualize_results = FALSE,
                                return_message = TRUE,
                                sort_by = "DateDesc",
                                maximum_records = 250) %>% 
                        distinct(urlArticle, .keep_all = TRUE), silent = TRUE)
  
  ## notify errors
  if (class(gdelt_pt_all) == "try-error") {
    
    # notify via email
    notifier(er_dta = gdelt_pt_all)
    
    return(tibble())
    
  }
  

  
  cat(">> GDELT query --> done.\n")
  return(gdelt_pt_all)
  
}


