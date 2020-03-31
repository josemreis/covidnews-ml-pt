#!/usr/local/bin/Rscript
#!/usr/bin/Rscript
#!/usr/bin/env Rscript
### Setting things up
###------------------------------------------------------------------------------------------
## conditional install of packs
packs <- c("tidyverse", "tidytext","hrbrthemes", "caret", "jsonlite", "tm", "SnowballC", "fastrtext")
for (pack in packs) {
  
  if (!requireNamespace(pack, quietly = TRUE)) {
    
    install.packages(pack)
    
  }
}
## Load the relevant packages
require(tidyverse)
require(tidytext)
require(tm)
require(SnowballC)
require(caret)
require(jsonlite)
require(fastrtext)
try(theme_set(theme_minimal()), silent = TRUE) # set plot theme

## working dir
root_dir <- getwd()
setwd("train")

### Preping the data
###----------------------------------------------------------------------------------
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

#run the function
untar("data/labeled_data/labeled_data.tar.xz", exdir = "data/labeled_data")
listed_files <- list.files("data/labeled_data/labeled_data", full.names = TRUE) %>%
  subset(., stringr::str_detect(., "json"))

# load
dta_raw <- map_df(listed_files, load_dta)

# export
write_csv(dta_raw, 
          "data/0_data_parsed.csv")

rm(list=ls())

### Text Pre-Processing
###----------------------------------------------------------------------------------------
# read latest dataset
dta_raw <- read_csv("data/0_data_parsed.csv")

## clean adn tidy
data_clean <- dta_raw %>%
  mutate(newspaper_id = str_extract(doc_id, "^[A-Z]+(?=[0-9])"),
         id = doc_id,
         lp_join = if_else(nchar(leading_paragraph) < 100,
                           remaining_content,
                           leading_paragraph),
         is_covid = ifelse(is_covid == TRUE,
                           "1", 
                           "0")) %>%
  unite(., col = "text", c("headlines", "lp_join"), sep = " ") %>%
  mutate(text = str_replace_all(text, "[^[:alnum:][:space:]\\-]|(f|ht)(tp)(s?)(\\://)(.*)[\\.|/](.*)", " ") %>%
           str_trim()) %>%
  select(id, newspaper_id, is_covid, text)

## Generating the text data: removing stopwords and stemming. Unit of analysis. Tokenization: bi-gram to 5-ngram. Again, unit of analysis: title + leading paragraph.
dta_pp <- map_df(2:5,
                 ~ unnest_tokens(data_clean, word, text,
                                   token = "ngrams", n = .x, to_lower = TRUE)) %>% 
  mutate(row_id = row_number()) %>%
  separate_rows(word, sep = "\\s+") %>%
  anti_join(tibble(word = stopwords("pt")), by = "word") %>%
  mutate(word = wordStem(word, language = "portuguese")) %>%
  group_by(row_id) %>% 
  mutate(word = paste(word, collapse = " ") %>%
           str_trim(.)) %>%
  ungroup() %>%
  distinct(row_id, .keep_all = TRUE) %>%
  count(id, word, sort = TRUE)
  
  write_csv(dta_pp, 
            "data/1_data_preproc.csv")

# Reduce dimensionality. Select tokens which appear in at least 10 articles.
words_5 <- dta_pp %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 5) %>%
  select(word)

# Right-join this to the data-fram. Next, we calculate the tf-idf and cast it to a document term matrix. We lost 5 articles due to the dimension reduction.
data_dtm <- dta_pp %>%
  right_join(words_5, by = "word") %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

# We create this meta data.frame which acts as a intermediate from our first data set since some articles might have disappeared completely after the reduction.
meta <- tibble(id = dimnames(data_dtm)[[1]]) %>%
  left_join(data_clean[!duplicated(data_clean$id), ], by = "id")

### Prepare ML data
###------------------------------------------------------------------------------------------
# Data split: 70% training, 30% test
set.seed(1234)
trainIndex <- createDataPartition(meta$is_covid, p = 0.7, list = FALSE, times = 1)

# Subset, turn to matrix and then to dataframe
data_df_train <- data_dtm[trainIndex, ] %>% 
  as.matrix() %>% 
  as.data.frame()

data_df_test <- data_dtm[-trainIndex, ] %>% 
  as.matrix() %>% 
  as.data.frame()

response_train <- meta$is_covid[trainIndex]

# Directory for the models.
if (!dir.exists("models")) {
  
  dir.create("models")
  
}  

### Modeling
###-------------------------------------------------------------------------------------------
### Support Vector Machines
# set the train control method
trctrl <- trainControl(method = "none")

# fit
svm_mod <- train(x = data_df_train,
                 y = as.factor(response_train),
                 method = "svmLinearWeights2",
                 trControl = trctrl,
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1))
# pred
svm_pred <- predict(svm_mod,
                    newdata = data_df_test)
# confusion matrix
svm_cm <- confusionMatrix(svm_pred, as.factor(meta[-trainIndex, ]$is_covid))
svm_cm

## export model
readr::write_rds(svm_mod,
     file = paste0("models/svmbaseline_", Sys.Date(), "_trained_on_",length(response_train),".rds"))

### Naive-bayes
# fit
nb_mod <- train(x = data_df_train,
                y = as.factor(response_train),
                method = "naive_bayes",
                trControl = trctrl,
                tuneGrid = data.frame(laplace = 0,
                                      usekernel = FALSE,
                                      adjust = FALSE))
# pred
nb_pred <- predict(nb_mod,
                   newdata = data_df_test)

# confusion matrix
nb_cm <- confusionMatrix(nb_pred, as.factor(meta[-trainIndex, ]$is_covid))
nb_cm

# export model
readr::write_rds(nb_mod,
     file = paste0("models/nbbaseline_", Sys.Date(), "_trained_on_",length(response_train),".rds"))

### LogitBoost
## fit
logitboost_mod <- train(x = data_df_train,
                        y = as.factor(response_train),
                        method = "LogitBoost",
                        trControl = trctrl)
# pred
logitboost_pred <- predict(logitboost_mod,
                           newdata = data_df_test)
# confusion matrix
logitboost_cm <- confusionMatrix(logitboost_pred, as.factor(meta[-trainIndex, ]$is_covid))
logitboost_cm

## export model
readr::write_rds(logitboost_mod,
     file = paste0("models/logitboostbaseline_", Sys.Date(), "_trained_on_",length(response_train),".rds"))

### Random Forests
## fit
rf_mod <- train(x = data_df_train, 
                y = as.factor(response_train), 
                method = "ranger",
                trControl = trctrl,
                tuneGrid = data.frame(mtry = floor(sqrt(dim(data_df_train)[2])),
                                      splitrule = "gini",
                                      min.node.size = 1))
# pred
rf_pred <- predict(rf_mod,
                   newdata = data_df_test)
# confusion matrix
rf_cm <- confusionMatrix(rf_pred, as.factor(meta[-trainIndex, ]$is_covid))
rf_cm

# export model
readr::write_rds(rf_mod,
     file = paste0("models/rfbaseline_", Sys.Date(), "_trained_on_",length(response_train),".rds"))

### fastText
## works only with full text, not tokens (doc embeddings generation)
# split them up using the index
train_txt <- data_clean[trainIndex,c("is_covid","text")] %>%
  distinct(text, is_covid, .keep_all = TRUE) 

test_txt <- data_clean[-trainIndex,c("is_covid","text")] %>%
  distinct(text, is_covid, .keep_all = TRUE)

# directory for the model
tmp_file_model <- paste0("models/fasttextbaseline", Sys.Date(), "_trained_on_",length(response_train))

## learn the training data
model_file <- build_supervised(documents = train_txt[['text']],
                               targets = train_txt[['is_covid']],
                               model_path = 'tmp_file_model',
                               dim = 100, lr = 1, epoch = 50, wordNgrams = 5, minCount = 3)

model <- load_model(model_file)
# pred
predictions <- predict(model, test_txt[['text']], k = 1)

## assign
test_txt$predicted_label <- map_chr(predictions, names)
test_txt$predicted_prob <- map_dbl(predictions, unname)

# confusion matrix
conf <-confusionMatrix(as.factor(test_txt$predicted_label), as.factor(test_txt$is_covid), dnn = c("Prediction", "Reference"))
conf

## Testing the quality of the embeddings, nearest neighbors.
get_nn(model, "covid", k = 12)
get_nn(model, "corona", k = 12)
get_nn(model, "sars", k = 12)
get_nn(model, "estado de emergencia", k = 12)
get_nn(model, "hospital", k = 12)
get_nn(model, "mascara", k = 12)
get_nn(model, "quarentena", k = 12)



## Comparing the models

## Model metrics as a data frame
mod_results <- map2_df(list(svm_cm, nb_cm, logitboost_cm, rf_cm, conf),
                       c("svm\nbaseline", "nb\nbaseline", "logitboost\nbaseline", "random forests\nbaseline", "fastText\nbaseline"),
                       ~ tibble(metric = c(names(.x$overall), names(.x$byClass)),
                                value = c(unname(.x$overall), unname(.x$byClass))) %>%
                         mutate(model = .y,
                                date = Sys.Date(),
                                train_n = length(response_train),
                                is_covid_class_prop = sum(as.numeric(response_train))/length(response_train)))

# model metrics dir
if (!dir.exists("models/model_metrics")) {
  
  dir.create("models/model_metrics")
  
}

# export
write_csv(mod_results,
          paste0("models/model_metrics/models_baseline_", Sys.Date(), "_trained_on_",length(response_train),".csv"))

# Plot model metrics
mod_results %>%
  filter(metric %in% c("F1", "Kappa", "Precision", "Recall")) %>%
  ggplot(aes(model, value)) +
  geom_point(size = 2.2, alpha = 0.7) +
  ylim(0, 1) +
  geom_hline(yintercept = mod_results$AccuracyNull[1],
             color = "red") +
  facet_wrap(~metric, nrow = "2") +
  ggtitle(paste0("Model metrics\n(", Sys.Date(), ", trained on ",length(response_train)," articles)"))

ggsave(paste0("models/model_metrics/models_baseline_", Sys.Date(), "_trained_on_",length(response_train),".png"), units="in", width=10, height=8)

## plot model metrics by model and size of training set
my_files <- list.files("models/model_metrics", full.names = TRUE) %>%
  subset(., str_detect(., "\\.csv"))

dta <- map_df(my_files, read_csv)

dta %>%
  filter(metric %in% c("F1", "Kappa")) %>%
  ggplot(aes(train_n, value, color = model)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~metric, nrow = 2) +
  ggtitle("Models and training-set")

ggsave(paste0("models/model_metrics/models_bytrain_", Sys.Date(), "_max_train_", unique(max(dta$train_n)),".png"), units="in", width=10, height=8)

### Tunning the parameters
###-------------------------------------------------------
### Optimizing the fast text and random forests
## Fast text
# Data split: 70% training, 30% test
set.seed(1234)
trainIndex <- createDataPartition(data_clean$is_covid, p = 0.7, list = FALSE, times = 1)

train_txt <- data_clean[trainIndex,c("is_covid","text")] %>%
  distinct(text, is_covid, .keep_all = TRUE) 

test_txt <- data_clean[-trainIndex,c("is_covid","text")] %>%
  distinct(text, is_covid, .keep_all = TRUE)


### Based on the following experiments: http://soner.in/fasttext-grid-search/
set.seed(1234)

fast_tunning <- pmap_df(
  list(dim = c(300, 300, 100, 100, 100, 100), 
       lr = c(0.25, 0.25, 0.25, 1, 0.05, 0.05), 
       epoch = c(100, 100, 100, 100, 500, 300), 
       ws = c(10, 25, 10, 10, 25, 30),
       wordNgrams = c(3, 3, 3, 5, 3, 6),
       model_n = seq(1, 6, 1)), function(dim, lr, epoch, wordNgrams, ws, model_n){
         
         tmp_file_model <- paste0("models/fasttexttune", Sys.Date(), "_trained_on_", nrow(train_txt), "_", model_n)
         
         model_file <- build_supervised(documents = train_txt[['text']],
                                        targets = train_txt[['is_covid']],
                                        model_path = 'tmp_file_model', maxn = 8, neg = 10,
                                        dim = dim, lr = lr, epoch = epoch, wordNgrams = wordNgrams, minCount = 3)
         
         model <- load_model(model_file)
         # pred
         predictions <- predict(model, test_txt[['text']], k = 1)
         ## get ham loss
         ham_loss <- get_hamming_loss(as.list(test_txt$is_covid), predictions = predictions)
         
         ## zip and remove
         zip(paste0(tmp_file_model, ".zip"), files = c(paste0(tmp_file_model, ".bin"), paste0(tmp_file_model, ".vec")))
         file.remove(c(paste0(tmp_file_model, ".bin"), paste0(tmp_file_model, ".vec")))
         
         ## return as tibble
         return(tibble(model_path = tmp_file_model,
                       ham_loss = ham_loss,
                       dim = dim,
                       lr = lr,
                       epoch = epoch,
                       wordNgrams = wordNgrams))
         
       })

### Unit of analysis: title + leading_paragraph
###---------------------------------------------------------------------------------------
dta_raw <- readr::read_csv("data/0_data_parsed.csv") %>%
  mutate(lp_join = if_else(nchar(leading_paragraph) < 100,
                           remaining_content,
                           leading_paragraph),
         is_covid = ifelse(is_covid == TRUE,
                           "1", 
                           "0")) %>%
  unite(., col = "text", c("headlines", "lp_join"), sep = " ") 

### Data partition
###----------------------------------------------------------------------------------------
# Subset, turn to matrix and then to dataframe
set.seed(1234)
trainIndex <- createDataPartition(dta_raw$is_covid, p = 0.7, list = FALSE, times = 1)
data_df_train <- dta_raw[trainIndex, c("text", "doc_id", "is_covid")] 
data_df_test <- dta_raw[-trainIndex, c("text", "doc_id", "is_covid")]


### Pre-processing text
###----------------------------------------------------------------------------------------
# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it_train <- itoken(data_df_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = data_df_train$doc_id,
                   progressbar = TRUE)

it_test <- itoken(data_df_test$text,
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = data_df_test$doc_id,
                  progressbar = TRUE)

## create vocabulary
vocab <- create_vocabulary(it_train, ngram = c(2L, 5L))
# prunning
vocab <- prune_vocabulary(vocab, term_count_min = 10, 
                          doc_proportion_max = 0.5)

vectorizer <- vocab_vectorizer(vocab)

## turn to document term matrix
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)

# define tfidf model
tfidf <- TfIdf$new()

# fit model to train data and transform train data with fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- transform(dtm_test, tfidf)

### final rf
rf_mod <- train(x = as.matrix(dtm_train_tfidf), 
                y = as.factor(data_df_train[['is_covid']]), 
                method = "ranger",
                trControl = trainControl(method="repeatedcv",
                                         number = 10,
                                         repeats = 3, 
                                         verboseIter = T),
                tuneGrid = tgrid)



plot(rf_mod, sub = "10-fold cross-validatio repeated 3 times\nsplit rule: Gini\n500 trees\nmin node size = 10")

rf_mod$bestTune

## Train using the  best model
tgrid <- expand.grid(
  mtry = 10,
  splitrule = "gini",
  min.node.size = 10
)

rf_mod <- train(x = as.matrix(dtm_train_tfidf), 
                y = as.factor(data_df_train[['is_covid']]), 
                method = "ranger",
                trControl = trainControl(method = "none"),
                tuneGrid = tgrid)



rf_pred <- predict(rf_mod, as.matrix(dtm_test_tfidf))

# confusion matrix
rf_cm <- confusionMatrix(rf_pred, as.factor(data_df_test[['is_covid']]))
rf_cm

readr::write_rds(rf_mod,
                 path = "models/rf-model.rds")


### Random forest
tgrid <- expand.grid(
  mtry = c(8, 16, 25, 30),
  splitrule = "gini",
  min.node.size = 10
)

rf_mod <- train(x = as.matrix(dtm_train_tfidf), 
                y = as.factor(data_df_train[['is_covid']]), 
                method = "ranger",
                trControl = trainControl(method="repeatedcv",
                                         number = 10,
                                         repeats = 3, 
                                         verboseIter = T),
                tuneGrid = tgrid)

plot(rf_mod)

rf_mod$bestTune

