### setting things up
###----------------------------------------------------------------------
require(dplyr)
require(purrr)

## list csv files
listed_files <- list.files("data", full.names = TRUE)
## load and rbind
dta_all <- map_df(listed_files, read.csv) %>% 
  distinct(url, .keep_all = TRUE)

#* @get /
function(my_dta = dta_all) {
    return(my_dta)
}