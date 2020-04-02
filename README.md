Covid-19 news aggregator for Portuguese news
================
José Maria Reis
4/2/2020

## Description

This repo contains a scraper pipeline which:

1.  Pulls all news from Portuguese domains using [GDELT’s
    API](https://www.gdeltproject.org/) given a certain time range using
    [gdeltr2’s wrapper functions](https://github.com/abresler/gdeltr2).
    Data is collected and updated every 40, 60 or 120 minutes, depending
    on time of day. For more details, see
    `scrape-parse-classify/1_gdelt_pull.R`

2.  Parses each news article with python using
    [news-please](https://github.com/fhamborg/news-please) or, if
    failling,
    [newspaper3k](https://newspaper.readthedocs.io/en/latest/), see
    `scrape-parse-classify/2_news_parse.py`

3.  Predicts whether or not a news article is about covid-19 using a
    trained random forests model (more details below),
    see`scrape-parse-classify/3_classify&push.R`

4.  Automatically pushes the data to `data/... .csv` in this repository
    as a individual csv file, see
    `scrape-parse-classify/3_classify&push.R`

To run the entire pipeline use
`scrape-parse-classify/4_get_covid19_news.R`.

## Details on the model

After some experimentation I settled on a random forests model using
[ranger](https://cran.r-project.org/web/packages/ranger/index.html).

  - **Corpus**: more than 6000 news articles from Portuguese outlets
    were collected using factiva. An article was considered to be about
    coronavirus if it contained the factiva label **Novel
    Coronaviruses**. You can find it in `train/data/labeled_data`
  - **Sampling**: the first sample strategy, up to roughly 3000 docs,
    involved making queries for news articles in random dates between
    01/01/2018 adn 01/10/2019 - not coronavirus-related - and random
    dates between 15/02/2020 and today using the above-mentioned label
    as a query parameters. Has this was leading to some overfitting a
    new strategy was adopted. I made queries for all news in a random
    date between 15/02/2020 and today, and labeled the data based on the
    presence of the coronavirus factiva label.
  - **Features** - unigram to 5-ngram tokenized words without stop-words
    represented as a tf-idf vector
  - **10-fold crossvalidatio repeated 3 times** for parameter tunning
  - Latest **model
specification**

| sample\_size | train\_prop\_covid | mtry | n\_tree | min\_node\_size | splitrule | model\_type    |
| -----------: | -----------------: | ---: | ------: | --------------: | :-------- | :------------- |
|         4110 |          0.2374787 |  202 |     500 |              20 | gini      | classification |

  - Latest **model
metrics**

| model\_accuracy | model\_kappa | model\_f1 | model\_precision | model\_recall |
| --------------: | -----------: | --------: | ---------------: | ------------: |
|       0.9465909 |    0.8515594 | 0.9650817 |        0.9622222 |     0.9679583 |

## Output example

``` r
dplyr::glimpse(read.csv(list.files("data", full.names = TRUE)[1]))
```

    ## Observations: 35
    ## Variables: 25
    ## $ prediction_covid_topic <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ title                  <fct> "Associação Empresarial da Região de Coimbra e…
    ## $ description            <fct> NA, NA, "O presidente da Associação Nacional d…
    ## $ authors                <fct> Notícias De Coimbra, Sul Informação, Global Me…
    ## $ date_download          <fct> 2020-04-02 21:00:34, 2020-04-02 21:00:35, 2020…
    ## $ maintext               <fct> "A NERC – Associação Empresarial da Região de …
    ## $ url                    <fct> https://www.noticiasdecoimbra.pt/associacao-em…
    ## $ gdelt_article_title    <fct> "Associao Empresarial da Região de Coimbra exi…
    ## $ date_publish           <fct> NA, 2020-04-02 16:52:05, 2020-04-02 19:09:00, …
    ## $ pred_input             <fct> "A NERC – Associação Empresarial da Região de …
    ## $ model_accuracy         <dbl> 0.9465909, 0.9465909, 0.9465909, 0.9465909, 0.…
    ## $ model_kappa            <dbl> 0.8515594, 0.8515594, 0.8515594, 0.8515594, 0.…
    ## $ model_f1               <dbl> 0.9650817, 0.9650817, 0.9650817, 0.9650817, 0.…
    ## $ model_precision        <dbl> 0.9622222, 0.9622222, 0.9622222, 0.9622222, 0.…
    ## $ model_recall           <dbl> 0.9679583, 0.9679583, 0.9679583, 0.9679583, 0.…
    ## $ model                  <fct> "Random Forests\n('ranger' package, R 3.5.3.)\…
    ## $ sample_size            <int> 4110, 4110, 4110, 4110, 4110, 4110, 4110, 4110…
    ## $ train_prop_covid       <dbl> 0.2374787, 0.2374787, 0.2374787, 0.2374787, 0.…
    ## $ mtry                   <int> 202, 202, 202, 202, 202, 202, 202, 202, 202, 2…
    ## $ n_tree                 <int> 500, 500, 500, 500, 500, 500, 500, 500, 500, 5…
    ## $ min_node_size          <int> 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20…
    ## $ splitrule              <fct> gini, gini, gini, gini, gini, gini, gini, gini…
    ## $ model_type             <fct> classification, classification, classification…
    ## $ fitted_on              <fct> 2020-04-02, 2020-04-02, 2020-04-02, 2020-04-02…
    ## $ txt_used               <fct> A NERC  Associação Empresarial da Região de Co…
