#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr  1 00:54:01 2020

@author: jmr
"""
from newsplease import NewsPlease, NewsArticle
from newspaper import Article
import pandas as pd

def parse_news(news_url = 'https://www.rt.com/news/203203-ukraine-russia-troops-border/'):
    try:
        article = NewsPlease.from_url(news_url)
        df = pd.DataFrame(article.get_dict())
        to_return = df[["authors", "date_download", "date_publish", "description", "maintext", "text", "title", "url"]]
        
        if len(to_return) == 0:
            news3k = Article(url = news_url,
                             language = "pt")
            news3k.download()
            news3k.parse()
            to_return = pd.DataFrame([{"authors": ", ".join(news3k.authors),
                                       "date_download": datetime.now(),
                                       "date_publish": news3k.publish_date,
                                       "description": news3k.meta_description,
                                       "maintext": news3k.text,
                                       "text": '',
                                       "title":news3k.title,
                                       "url":news_url}])
    except:
        to_return = pd.DataFrame()
    return to_return

