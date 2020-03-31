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