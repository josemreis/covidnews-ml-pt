Covid-19 news aggregator for Portuguese news
================

## Description

This repo contains a scraper pipeline which:

1.  Pulls all news from available Portuguese domains using [GDELT’s
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
    Coronaviruses**. You can find it in `train/data/labeled_data`. For
    collecting the corpus, empty queries for Newspapers in Europe in
    Portuguesew were used.
  - **Sampling**: the first sample strategy, up to roughly 3000 docs,
    involved making queries for news articles in random dates between
    01/01/2018 adn 01/10/2019 - not coronavirus-related - and random
    dates between 15/02/2020 and today using the above-mentioned label
    as a query parameters. Has this was leading to some overfitting a
    new strategy was adopted. I made queries for all news in a random
    date between 15/02/2020 and today, and labeled the data based on the
    presence of the coronavirus factiva label.
  - **Features** - unigram to 5-ngram tokenized words without stop-words
    and stemmed represented as a tf-idf vector. Only words which
    appeated in at least 20 documents were kept.
  - **10-fold crossvalidation repeated 3 times** for parameter tunning
  - Latest **model
specification**

| sample\_size | train\_prop\_covid | mtry | n\_tree | min\_node\_size | splitrule | model\_type    |
| -----------: | -----------------: | ---: | ------: | --------------: | :-------- | :------------- |
|         5479 |          0.4347048 |  204 |     500 |              20 | gini      | classification |

  - Latest **model
metrics**

| model\_accuracy | model\_kappa | model\_f1 | model\_precision | model\_recall |
| --------------: | -----------: | --------: | ---------------: | ------------: |
|       0.9646357 |     0.928081 | 0.9686674 |        0.9704992 |     0.9668425 |

## Output example

``` r
dplyr::glimpse(read.csv(list.files("data", full.names = TRUE)[1]))
```

    ## Observations: 80
    ## Variables: 39
    ## $ prediction_covid_topic [3m[38;5;246m<int>[39m[23m 1, 1, 1, 1, 1, 1,…
    ## $ title                  [3m[38;5;246m<fct>[39m[23m "\"Podem ser libe…
    ## $ description            [3m[38;5;246m<fct>[39m[23m "Ministra da Just…
    ## $ authors                [3m[38;5;246m<fct>[39m[23m Global Media Grou…
    ## $ date_download          [3m[38;5;246m<fct>[39m[23m 2020-04-03 02:07:…
    ## $ maintext               [3m[38;5;246m<fct>[39m[23m "Francisca van Du…
    ## $ url                    [3m[38;5;246m<fct>[39m[23m https://www.tsf.p…
    ## $ gdelt_title            [3m[38;5;246m<fct>[39m[23m " ″ Podem ser lib…
    ## $ gdelt_url              [3m[38;5;246m<fct>[39m[23m https://www.tsf.p…
    ## $ date_publish           [3m[38;5;246m<fct>[39m[23m 2020-04-02 23:44:…
    ## $ modeSearch             [3m[38;5;246m<fct>[39m[23m artlist, artlist,…
    ## $ sourcecountrySearch    [3m[38;5;246m<fct>[39m[23m PO, PO, PO, PO, P…
    ## $ termSearch             [3m[38;5;246m<lgl>[39m[23m NA, NA, NA, NA, N…
    ## $ periodtimeSearch       [3m[38;5;246m<fct>[39m[23m 4 hours, 4 hours,…
    ## $ isOR                   [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FAL…
    ## $ countMaximumRecords    [3m[38;5;246m<int>[39m[23m 250, 250, 250, 25…
    ## $ urlGDELTV2FTAPI        [3m[38;5;246m<fct>[39m[23m https://api.gdelt…
    ## $ urlArticleMobile       [3m[38;5;246m<fct>[39m[23m https://www.tsf.p…
    ## $ datetimeArticle        [3m[38;5;246m<fct>[39m[23m 2020-04-02T21:15:…
    ## $ urlImage               [3m[38;5;246m<fct>[39m[23m https://static.gl…
    ## $ domainArticle          [3m[38;5;246m<fct>[39m[23m tsf.pt, ojogo.pt,…
    ## $ languageArticle        [3m[38;5;246m<fct>[39m[23m Portuguese, Portu…
    ## $ countryArticle         [3m[38;5;246m<fct>[39m[23m Portugal, Portuga…
    ## $ pred_input             [3m[38;5;246m<fct>[39m[23m "Francisca van Du…
    ## $ model_accuracy         [3m[38;5;246m<dbl>[39m[23m 0.9465909, 0.9465…
    ## $ model_kappa            [3m[38;5;246m<dbl>[39m[23m 0.8515594, 0.8515…
    ## $ model_f1               [3m[38;5;246m<dbl>[39m[23m 0.9650817, 0.9650…
    ## $ model_precision        [3m[38;5;246m<dbl>[39m[23m 0.9622222, 0.9622…
    ## $ model_recall           [3m[38;5;246m<dbl>[39m[23m 0.9679583, 0.9679…
    ## $ model                  [3m[38;5;246m<fct>[39m[23m "Random Forests\n…
    ## $ sample_size            [3m[38;5;246m<int>[39m[23m 4110, 4110, 4110,…
    ## $ train_prop_covid       [3m[38;5;246m<dbl>[39m[23m 0.2374787, 0.2374…
    ## $ mtry                   [3m[38;5;246m<int>[39m[23m 202, 202, 202, 20…
    ## $ n_tree                 [3m[38;5;246m<int>[39m[23m 500, 500, 500, 50…
    ## $ min_node_size          [3m[38;5;246m<int>[39m[23m 20, 20, 20, 20, 2…
    ## $ splitrule              [3m[38;5;246m<fct>[39m[23m gini, gini, gini,…
    ## $ model_type             [3m[38;5;246m<fct>[39m[23m classification, c…
    ## $ fitted_on              [3m[38;5;246m<fct>[39m[23m 2020-04-02, 2020-…
    ## $ txt_used               [3m[38;5;246m<fct>[39m[23m Francisca van Dun…
