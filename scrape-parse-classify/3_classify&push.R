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

## source dbpedia ner
source("train/dbpedia_ner.R")

## load the random forests model and model metrics
model <- readRDS("train/final_model/rf-model.rds")
model_metrics <- read.csv("train/final_model/rf-model-metrics.csv")
## vectorizer
vocab <- readRDS("train/final_model/rf-vectorizer.rds")
## config git folder
git2r::config(user.name = "josemreis",user.email = readLines("/home/jmr/ile_mail.txt"))
## git key
git_key <- readLines("/home/jmr/github_pass.txt")
## named entity coutns
tidy_types <- read_csv("train/data/dbpedia_entity-type_counts.csv")

# load the training data
training_txt <- readr::read_csv("train/data/0_data_parsed.csv") %>%
  mutate(lp_join = if_else(nchar(leading_paragraph) < 600,
                           remaining_content,
                           leading_paragraph),
         is_covid = ifelse(is_covid == TRUE,
                           "1",
                           "0")) %>%
  unite(., col = "text", c("headlines", "lp_join"), sep = " ") %>%
  distinct(text, is_covid, .keep_all = TRUE) %>%
  filter(!is.na(is_covid)) %>%
  dplyr::pull("text")

## text input helper function
##------------------------------------------------------------------------
## pre-train tfidf model
## stemmer
stemmer <- function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="en")
}

## define the function
train_tfidf_model <- function(txt = training_txt) {
  
  ## tokenizing
  prep_fun = tolower
  tok_fun = stemmer
  
  it <- itoken(txt,
               preprocessor = prep_fun, 
               tokenizer = tok_fun,
               progressbar = TRUE)
  
  ## vectorizer, pass it to main environment
  vectorizer <<- vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- create_dtm(it, vectorizer)
  
  # define the new tfidf model 
  tfidf <- TfIdf$new()
  
  # fit model to train data and transform train data with fitted model
  dtm_text_tfidf <- fit_transform(dtm_text, tfidf) 
  
  ## pass to environment where function is called (pass it to test set)
  trained_tfidf <<- tfidf
  
}
## train a tfidf model with corpus, tfidf model object passed this environment
train_tfidf_model()

### prep input data
prep_input <- function(txt) {
  
  ## the server has a limitfor character in the get request. Server limit seems to be 5000.
  entity_txt <- ifelse(nchar(txt) > 5000,
                        str_sub(txt, start = 1, end = 5000),
                        txt)
  
  ## get dbpedia entities
  entities <- try(get_dbpedia_entities(txt = entity_txt, doc_id = "fooh"), silent = TRUE)
  
  if (class(entities) == "try-error" || is.null(entities) || nrow(entities) == 0){
    
    # random empty tibble
    entities <- tibble(types = "fooh")
    
  }
  
  entities <- entities %>%
    mutate(entity_types_all = str_extract_all(types, "(?<=DBpedia\\:)\\w+")) %>%
    select(entity_types_all) %>%
    dplyr::pull(entity_types_all) %>%
    map_df(.,
           ~ tibble(entity_type = .x)) %>%
    count(entity_type) %>%
    pivot_wider(names_from = entity_type, values_from = n, names_prefix = "entity_count_") %>%
    mutate(doc_id = "fooh")
  
  ## add remaining entities with count == 0
  other_types <- tidy_types[1,] %>%
    mutate(doc_id = "fooh")
  
  entities_all <- left_join(entities, other_types) %>%
    select(-doc_id) %>%
    mutate_all(., ~ ifelse(is.na(.), 0, .)) 
  
  if (nrow(entities_all) == 0) {
    
    entities_all[1,] <- 0
    entities_all <- entities_all %>%
      select(-n)
    
  }
  
  ## pre-process the text
  prep_fun = tolower
  tok_fun = stemmer
  
  it <- text2vec::itoken(txt,
                         preprocessor = prep_fun, 
                         tokenizer = tok_fun,
                         progressbar = TRUE,
                         id = "target_text")
  
  ## vectorizer
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- text2vec::create_dtm(it, vectorizer) 
  
  ## tfidf vectorization using the tfidf dtm used for training
  dtm_text_tfidf <- transform(dtm_text, trained_tfidf) %>%
    as.matrix() %>%
    as.data.frame() %>% ## add the remaining vars
    cbind(entities_all) 
  
  ## return
  return(dtm_text_tfidf)
  
}

## Classify
##---------------------------------------------------------------------------
covid_classifier <- function(txt) {
  
  clean <- txt %>% 
    stringi::stri_replace_all_regex(., '[[:cntrl:]]+|(?<!\\{|\\{\\"[a-z]{3,10}|\\:\\s{0,10})\\"(?!\\}|\\:)|[^[:alnum:][:space:]\\-]', '') %>%
    stringi::stri_enc_toutf8(.) %>%
    trimws()
  
  if (!is.na(clean) && nchar(clean) > 80) {
  
    txt_dtm <- prep_input(txt = clean) %>%
      mutate_all(., ~ ifelse(is.na(.) , 0, .)) 
    
    ## predict
    my_pred <- predict(model, newdata = txt_dtm)
    
  } else {
    
    my_pred <- NA
    
  }
    ## get mod metrics
    to_return <- model_metrics %>%
      mutate(about_covid = my_pred) %>%
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
  
  cat("\n Data pushed to remote github https://github.com/josemreis/covidnews-ml-pt")
  
}
