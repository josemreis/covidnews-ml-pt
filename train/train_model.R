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
### fastText
set.seed(1234)
trainIndex <- createDataPartition(dta_raw$is_covid, p = 0.7, list = FALSE, times = 1)
## get training
train_txt <- dta_raw[trainIndex,c("is_covid","text")] 
test_txt <- dta_raw[-trainIndex,c("is_covid","text")] 


# directory for the model
tmp_file_model <- "models/fasttext_final"

## learn the training data
build_supervised(documents = dta_raw[['text']],
                 targets = dta_raw[['is_covid']],
                 model_path = 'tmp_file_model', loss = "softmax", ws = 10,
                 dim = 100, lr = 1, epoch = 100, wordNgrams = 5, minCount = 10)


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
  
  saveRDS(vocab, file = "models/rf-vectorizer.rds")
  
  ## vectorizer
  vectorizer <- vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- create_dtm(it, vectorizer)
  
  ## return
  return(dtm_text)
  
}

# Train using the  best model
tgrid <- expand.grid(
  mtry = c(16, 25, 30),
  splitrule = "gini",
  min.node.size = 10
)

## fit the model
# prep train data
train <- dta_raw[trainIndex,]
dtm <- prep_train(train$text) %>%
  as.matrix() %>%
  as.data.frame()

rf_mod <- train(x = as.data.frame(dtm), 
                y = as.factor(train[['is_covid']]), 
                method = "ranger",
                trControl = trainControl(method="repeatedcv",
                                         number = 10,
                                         repeats = 3, 
                                         verboseIter = T),
                tuneGrid = tgrid)

# prep test
prep_test <- function(txt) {
  
  prep_fun = tolower
  tok_fun = word_tokenizer
  
  it <- itoken(txt,
               preprocessor = prep_fun, 
               tokenizer = tok_fun,
               progressbar = TRUE)
  
  ## vectorizer
  vocab <- readRDS("models/rf-vectorizer.rds")
  vectorizer <- vocab_vectorizer(vocab)
  
  ## turn to document term matrix
  dtm_text <- create_dtm(it, vectorizer)
  
  ## return
  return(dtm_text)
  
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

readr::write_csv(mod_metrics,
                 path = "models/rf-model-metrics.csv")



