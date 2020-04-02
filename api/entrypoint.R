library(plumber)

r <- plumb("api/covidnewsplumber.R")

r$run(port=8000)