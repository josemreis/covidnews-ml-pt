### Named Entity Recognition using dbpedia

### Setting things up
###---------------------------------------------------------------------------------------
require(httr)
require(tidyverse)

# ## load the latest dataset
# dta_raw <- readr::read_csv("train/data/0_data_parsed.csv") %>%
#   mutate(text = leading_paragraph) %>%
#   distinct(text, is_covid, .keep_all = TRUE) %>%
#   filter(!is.na(is_covid) & !is.na(text))

### wrapper function
get_dbpedia_entities <- function(txt, doc_id) {
  
   req <- tryCatch(RETRY(verb = "GET",
                          url = "https://api.dbpedia-spotlight.org/pt/annotate",
                          query = list(text = txt,
                                       confidence = 0.5,
                                       types = "DBpedia:Activity, DBpedia:AnatomicalStructure, DBpedia:ChemicalSubstance, DBpedia:Disease, DBpedia:Drug, DBpedia:Organisation, DBpedia:Person, DBpedia:Protein, DBpedia:Work"),
                          accept_json(),
                          times = 10,
                          pause_min = 15),
                    error = function(e) NULL)
    
    if (!is.null(req)){
      
      ## parse
      res <- try(jsonlite::fromJSON(httr::content(req, as = "text")) %>%
                   `[[`("Resources") %>%
                   as_tibble() %>%
                   set_names(., 
                             str_remove_all(names(.), "@")) %>%
                   mutate(doc_id = doc_id), silent = TRUE)
      
      if (class(res) == "try-error") {
        
        res <- NULL
        cat("\nParsing error.\n")
        
      } 
      
      if (nrow(res) == 0) {
        
        res <- NULL
        cat("\nNo entities.\n")
        
      } 
      
      
    } else {
      
      res <- NULL
      cat(paste("\nRequest refused.", req$status, sep = "\n"))
      
    }
    
    print(res)
    
    Sys.sleep(1)
    return(res)  
  
}

# ### Extract
# covid_ner <- map2_df(dta_raw$text[!dta_raw$doc_id %in% current_entities$doc_id], dta_raw$doc_id[!dta_raw$doc_id %in% current_entities$doc_id],
#                      ~ get_dbpedia_entities(txt = .x, doc_id = .y))
# 
# # join
# to_join <- select(dta_raw, doc_id, is_covid)
# joined <- left_join(covid_ner, to_join)
# 
# ## join to previous
# news_entities <- rbind(current_entities, joined)
# 
# # export 
# write_csv(news_entities,
#           "train/data/dbpedia_named_entities.csv")
# 
# joined %>%
#   group_by(is_covid) %>%
#   count(surfaceForm) %>%
#   arrange(desc(n))  %>%
#   top_n(., n = 10, wt = n) %>%
#   ungroup() %>%
#   ggplot(aes(surfaceForm, n)) + 
#   geom_bar(stat = "identity") + 
#   facet_wrap(~is_covid, scales = "free") + 
#   coord_flip()
#   
# ### Add category based counts for the entities
# entities <- readr::read_csv("train/data/dbpedia_named_entities.csv")
# 
# ## dbpedia:CATEGORY based tidy dataframe
# entity_types <- entities %>%
#   mutate(entity_types_all = str_extract_all(types, "(?<=DBpedia\\:)\\w+")) %>%
#   select(doc_id, entity_types_all)
# 
# ## tidy and count
# tidy_types <- map2_df(entity_types$entity_types_all, entity_types$doc_id,
#                       ~ tibble(entity_type = .x) %>%
#                         mutate(doc_id = .y)) %>%
#   count(doc_id, entity_type) %>%
#   pivot_wider(names_from = entity_type, values_from = n, names_prefix = "entity_count_") %>%
#   mutate_all(., ~ ifelse(is.na(.), 0, .))
# 
# ## export
# write_csv(tidy_types,
#           "train/data/dbpedia_entity-type_counts.csv")
