#!/usr/local/bin/Rscript
#!/usr/bin/Rscript
#!/usr/bin/env Rscript
### Setting things up
###------------------------------------------------------------------------------------------
## conditional install of packs
packs <- c("tidyverse", "caret", "jsonlite", "fastrtext", "text2vec", "sparsity")
for (pack in packs) {
  
  if (!requireNamespace(pack, quietly = TRUE)) {
    
    install.packages(pack)
    
  }
}
## Load the relevant packages
require(tidyverse)
require(caret)
require(jsonlite)
library(ranger)
require(text2vec)
require(git2r)
try(theme_set(theme_minimal()), silent = TRUE) # set plot theme

## working dir
root_dir <- here::here()

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


# list datasets
listed_files <- list.files("train/data/labeled_data", full.names = TRUE) %>%
  subset(., stringr::str_detect(., "json"))

# load
dta_raw <- map_df(listed_files, load_dta)

# export
write_csv(dta_raw, 
          "train/data/0_data_parsed.csv")

## prep
# dta_raw <- readr::read_csv("train/data/0_data_parsed.csv") %>%
#   mutate(lp_join = if_else(nchar(leading_paragraph) < 600,
#                            remaining_content,
#                            leading_paragraph),
#          is_covid = ifelse(is_covid == TRUE,
#                            "1", 
#                            "0")) %>%
#   unite(., col = "text", c("headlines", "lp_join"), sep = " ") %>%
#   distinct(text, is_covid, .keep_all = TRUE) %>%
#   filter(!is.na(is_covid))

dta_raw <- readr::read_csv("train/data/0_data_parsed.csv") %>%
  mutate(text = remaining_content) %>%
  distinct(text, is_covid, .keep_all = TRUE) %>%
  filter(!is.na(is_covid)) 

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
  
  vocab <- prune_vocabulary(vocab, term_count_min = 20, doc_proportion_min = 0.001)
  
  saveRDS(vocab, file = "train/final_model/rf-vectorizer.rds")
  
  ## vectorizer
  vectorizer <<- vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- create_dtm(it, vectorizer)
  
  tfidf <- TfIdf$new()
  # fit model to train data and transform train data with fitted model
  dtm_text_tfidf <- fit_transform(dtm_text, tfidf)
  ## pass to environment where function is called (pass it to test set)
  trained_tfidf <<- tfidf
  ## return
  return(dtm_text_tfidf)
  
}

## fit the model
# prep train data
train_df <- dta_raw[trainIndex,]
clean_train <- prep_train(train_df$text)
# export the dtm
sparsity::write.svmlight(clean_train, labelVector = as.numeric(train_df$is_covid), file = "train/final_model/train_dtm.svmlight")
# reformat for the model
dtm <- clean_train %>%
  as.matrix() %>%
  as.data.frame()

# Tune randomly selected predictors (min.node.size seems to be optima at 20 for most mtry)
# set.seed(1234)
tgrid <- expand.grid(
  mtry = c(floor(sqrt(ncol(dtm))), floor(sqrt(ncol(dtm))) * 2, floor(sqrt(ncol(dtm))) * 3),
  splitrule = "gini",
  min.node.size = 20
)
tcntrl <- trainControl(method="repeatedcv",
                       number = 10,
                       repeats = 3,
                       verboseIter = T)

rf_mod <- caret::train(x = dtm,
                y = as.factor(train_df[["is_covid"]]),
                method = "ranger",
                trControl = tcntrl,
                tuneGrid = tgrid)


# final model
rf_mod <- caret::train(x = dtm,
                       y = as.factor(train_df[["is_covid"]]),
                       method = "ranger",
                       trControl = tcntrl,
                       tuneGrid = tgrid)

# prep test
prep_test <- function(txt) {
  
  prep_fun = tolower
  tok_fun = word_tokenizer
  
  it <- itoken(txt,
               preprocessor = prep_fun, 
               tokenizer = tok_fun,
               progressbar = TRUE)
  
  ## turn to document term matrix
  dtm_text_tfidf <- create_dtm(it, vectorizer) %>%
    transform(trained_tfidf)
  
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
                 path = "train/final_model/rf-model.rds")

## model-metrics-final
mod_metrics <- rbind(tibble(value = rf_cm$overall, metric = names(rf_cm$overall)),
                     tibble(value = rf_cm$byClass, metric = names(rf_cm$byClass))) %>%
  filter(metric %in% c("Accuracy", "Kappa", "F1", "Precision", "Recall")) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(model = "Random Forests\n('ranger' package, R 3.5.3.)\nWright MN, Ziegler A (2017). “ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R.” Journal of Statistical Software, 77(1), 1–17. doi: 10.18637/jss.v077.i01.",
         sample_size = rf_mod$finalModel$num.samples,
         train_prop_covid = sum(dta_raw$is_covid == TRUE)/nrow(dta_raw),
         mtry = rf_mod$finalModel$mtry,
         n_tree = rf_mod$finalModel$num.trees,
         min_node_size = 20,
         splitrule = "gini",
         model_type = "classification",
         fitted_on = Sys.Date()) %>%
  select(model_accuracy = Accuracy, model_kappa = Kappa, model_f1 = F1, model_precision = Precision, model_recall = Recall, everything())

png(filename = "train/final_model/rf-params.png", width = 800, height = 600)
plot(rf_mod, metric = "Kappa")
dev.off()

readr::write_csv(mod_metrics,
                 path = "train/final_model/rf-model-metrics.csv")

## for checking stability as training data gets fed into the model
readr::write_csv(mod_metrics,
                 path = paste("train/final_model/sample_level_metrics/rf-model-metrics","_sample_n_", nrow(train_df), ".csv"))

## push the new model
# stage and commit changes
add(repo = ".",
    path = ".")

commit(repo = ".", 
       message = paste0("Retrained model " , Sys.time()),
       all = TRUE)

## push it
push(object = ".",
     credentials = cred_user_pass(username = "josemreis",
                                  password = readLines("/home/jmr/github_pass.txt")))

