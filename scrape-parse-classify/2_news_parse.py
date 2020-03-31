#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr  1 00:54:01 2020

@author: jmr
"""
from newsplease import NewsPlease, NewsArticle
import pandas as pd

def parse_news(news_url = 'https://www.rt.com/news/203203-ukraine-russia-troops-border/'):
    try:
        article = NewsPlease.from_url(news_url)
        to_return = pd.DataFrame(article.get_dict())
    except:
        to_return = pd.DataFrame()
    return to_return

