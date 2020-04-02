Covid-19 news aggregator for Portuguese news
================
José Maria Reis
4/2/2020

## Description

This repo contains a scraper pipeline which:

1.  Pulls all news from Porguese domains from [GDELT’s
    API](https://www.gdeltproject.org/) given a certain time range using
    [gdeltr2’s wrapper functions](https://github.com/abresler/gdeltr2).
    For more details, see `scrape-parse-classify/1_gdelt_pull.R`

2.  Parses each news’ url in python using
    [news-please](https://github.com/fhamborg/news-please) or, if
    failling,
    [newspaper3k](https://newspaper.readthedocs.io/en/latest/), see
    `scrape-parse-classifcy/2_news_parse.py`

3.  Predicts whether or not a news article is about covid-19 using a
    trained random forests model (more details below),
    see`scrape-parse-classify/3_classify&push.R`

4.  Automatically pushes the data to `data/... .csv` in this repository
    as a individual csv file, see
    `scrape-parse-classifcy/3_classify&push.R`

To run the entire pipeline use
`scrape-parse-classifcy/4_get_covid19_news.R`.

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
  - **Final
specification**

<!-- end list -->

``` r
knitr::kable(read.csv("train/final_model/rf-model-metrics.csv")[,c(7:13)], format = "markdown")
```

| sample\_size | train\_prop\_covid | mtry | n\_tree | min\_node\_size | splitrule | model\_type    |
| -----------: | -----------------: | ---: | ------: | --------------: | :-------- | :------------- |
|         4110 |          0.2374787 |  202 |     500 |              20 | gini      | classification |

  - Latest **model
metrics**

<!-- end list -->

``` r
knitr::kable(read.csv("train/final_model/rf-model-metrics.csv")[,c(1:5)], format = "markdown")
```

| model\_accuracy | model\_kappa | model\_f1 | model\_precision | model\_recall |
| --------------: | -----------: | --------: | ---------------: | ------------: |
|       0.9465909 |    0.8515594 | 0.9650817 |        0.9622222 |     0.9679583 |
