Covid-19 news aggregator for Portuguese news
================

## Description

This repo contains a scraper pipeline which:

1.  Pulls all news from available Portuguese domains using [GDELT’s
    API](https://www.gdeltproject.org/) given a certain time range using
    [gdeltr2’s wrapper functions](https://github.com/abresler/gdeltr2).
    Data is collected and updated every every 2 hours. For more details,
    see `scrape-parse-classify/1_gdelt_pull.R`

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
    collecting the corpus, empty queries (“e”) for Newspapers in Europe
    in Portuguese were used.
  - **Sampling**: I made queries for all news in a random
    date between  01/01/2018 and 01/04/2020 in Factiva and labeled the data based on the
    presence of the coronavirus factiva label ('Novel Coronaviruses') and its date.
  - **Features** - unigram to 5-ngram tokenized words without stop-words
    and stemmed represented as a tf-idf vector. Only words which
    appeated in at least 20 documents were kept. Furthermore, using
    [dbpedia’s spotlight api](https://www.dbpedia-spotlight.org/api)
    extracted all named entities present in the document, aggregated
    them by dbpedia macro-category, and added the counts for each
    category as features.
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

    ## Observations: 24
    ## Variables: 38
    ## $ prediction_covid_topic <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ title                  <fct> "Testes \"de prevenção\" em lares arrancaram e…
    ## $ description            <fct> "Os testes de \"prevenção\" do covid-19 arranc…
    ## $ authors                <fct> Céu Neves, Global Media Group, Global Media Gr…
    ## $ date_download          <fct> 2020-04-05 13:40:19, 2020-04-05 13:40:20, 2020…
    ## $ date_publish           <fct> 2020-04-05 11:15:00, 2020-04-05 11:33:00, 2020…
    ## $ maintext               <fct> "Os testes ao covid-19 em lares de idosos, lan…
    ## $ url                    <fct> https://www.dn.pt/pais/testes-de-prevencao-em-…
    ## $ gdelt_title            <fct> "Testes ″ de preveno ″ em lares arrancaram em …
    ## $ gdelt_url              <fct> https://www.dn.pt/pais/testes-de-prevencao-em-…
    ## $ modeSearch             <fct> artlist, artlist, artlist, artlist, artlist, a…
    ## $ sourcecountrySearch    <fct> PO, PO, PO, PO, PO, PO, PO, PO, PO, PO, PO, PO…
    ## $ termSearch             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ periodtimeSearch       <fct> 100 minutes, 100 minutes, 100 minutes, 100 min…
    ## $ isOR                   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    ## $ countMaximumRecords    <int> 250, 250, 250, 250, 250, 250, 250, 250, 250, 2…
    ## $ urlGDELTV2FTAPI        <fct> https://api.gdeltproject.org/api/v2/doc/doc?qu…
    ## $ urlArticleMobile       <fct> https://www.dn.pt/pais/amp/testes-de-prevencao…
    ## $ datetimeArticle        <fct> 2020-04-05T09:15:00Z, 2020-04-05T09:15:00Z, 20…
    ## $ urlImage               <fct> "https://static.globalnoticias.pt/dn/image.asp…
    ## $ domainArticle          <fct> dn.pt, tsf.pt, tsf.pt, observador.pt, observad…
    ## $ languageArticle        <fct> Portuguese, Portuguese, Portuguese, Portuguese…
    ## $ countryArticle         <fct> Portugal, Portugal, Portugal, Portugal, Portug…
    ## $ pred_input             <fct> "Os testes ao covid-19 em lares de idosos, lan…
    ## $ model_accuracy         <dbl> 0.9646357, 0.9646357, 0.9646357, 0.9646357, 0.…
    ## $ model_kappa            <dbl> 0.928081, 0.928081, 0.928081, 0.928081, 0.9280…
    ## $ model_f1               <dbl> 0.9686674, 0.9686674, 0.9686674, 0.9686674, 0.…
    ## $ model_precision        <dbl> 0.9704992, 0.9704992, 0.9704992, 0.9704992, 0.…
    ## $ model_recall           <dbl> 0.9668425, 0.9668425, 0.9668425, 0.9668425, 0.…
    ## $ model                  <fct> "Random Forests\n('ranger' package, R 3.5.3.)\…
    ## $ sample_size            <int> 5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479…
    ## $ train_prop_covid       <dbl> 0.4347048, 0.4347048, 0.4347048, 0.4347048, 0.…
    ## $ mtry                   <int> 204, 204, 204, 204, 204, 204, 204, 204, 204, 2…
    ## $ n_tree                 <int> 500, 500, 500, 500, 500, 500, 500, 500, 500, 5…
    ## $ min_node_size          <int> 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20…
    ## $ splitrule              <fct> gini, gini, gini, gini, gini, gini, gini, gini…
    ## $ model_type             <fct> classification, classification, classification…
    ## $ fitted_on              <fct> 2020-04-05, 2020-04-05, 2020-04-05, 2020-04-05…
