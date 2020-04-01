#!/usr/local/bin/Rscript
######################################################################################

# file: 3_classify&push.R

# date: 01/04/2020

# Purpose: classify whether a news article is about covid19 if yes export and push it to github

######################################################################################
### Setting things up
#-----------------------------------------------------------------------------------
### packs
library(tidyverse)
library(git2r)
library(caret)
library(text2vec)

## load the random forests model and model metrics
model <- readRDS("train/final_model/rf-model.rds")
model_metrics <- read.csv("train/final_model/rf-model-metrics.csv")
## vectorizer
vocab <- readRDS("train/final_model/rf-vectorizer.rds")
## config git folder
git2r::config(user.name = "josemreis",user.email = readLines("/home/jmr/ile_mail.txt"))
## git key
git_key <- readLines("/home/jmr/github_pass.txt")



## text input helper function
##------------------------------------------------------------------------
### prep input data
prep_input <- function(txt) {
  
  prep_fun = tolower
  tok_fun = text2vec::word_tokenizer
  
  it <- text2vec::itoken(txt,
                         preprocessor = prep_fun, 
                         tokenizer = tok_fun,
                         progressbar = TRUE)
  
  ## vectorizer
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- text2vec::create_dtm(it, vectorizer) %>%
    as.matrix() %>%
    as.data.frame()
  
  ## return
  return(dtm_text)
  
}

## Classify
##---------------------------------------------------------------------------
covid_classifier <- function(txt) {
  
  clean <- txt %>% 
    stringi::stri_replace_all_regex(., '[[:cntrl:]]+|(?<!\\{|\\{\\"[a-z]{3,10}|\\:\\s{0,10})\\"(?!\\}|\\:)|[^[:alnum:][:space:]\\-]', '') %>%
    stringi::stri_enc_toutf8(.) %>%
    trimws()
  
  if (!is.na(txt) && nchar(txt) > 80) {
  
    txt_dtm <- prep_input(txt = clean)
    
    ## predict
    my_pred <- caret::predict.train(model, newdata = txt_dtm)
    
  } else {
    
    my_pred <- NA
    
  }
    ## get mod metrics
    to_return <- model_metrics %>%
      mutate(about_covid = my_pred,
             txt_used = clean) %>%
      select(about_covid, everything())
    
    return(to_return)
    
  }

## automated github_push
##------------------------------------------------------------------------

## git push to https://github.com/josemreis/covidnews-ml-pt
automated_push <- function(filename) {
  
  # comit message
  commit_msg <- paste("pushing",
                      filename, 
                      "at",
                      format(Sys.time(), "%Y-%d-%m-%H-%M"),
                      sep = " ")
  
  # stage and commit changes
  add(repo = ".",
      path = filename)
  
  commit(repo = ".", 
         message = commit_msg,
         all = TRUE)
  
  ## push it
  push(object = ".",
       credentials = cred_user_pass(username = "josemreis",
                                    password = git_key))
  
  
}
