library(tidyverse)
source("connect_database.R")
source("transformation_functions.R")


consolidate_data <- function(table_name)
{
  data_raw <- tbl(con, table_name) %>% 
    collect()
  
  analyzation <- tbl(con, "openai_analyzation_corpus") %>% 
    select(-cf2, -af5, -nf3) %>% 
    collect() %>%
    select(where(~ all(!is.na(.)))) %>% 
    aggregate_model_by_hash()
  
  data <- left_join(data_raw, analyzation, by = c("hash" = "hash")) %>%
    select(-text_numtokens) %>% 
    select(where(~ all(!is.na(.))))

  return(data)
}

consolidate_data_with_nf3 <- function(table_name)
{
  data_raw <- tbl(con, table_name) %>% 
    collect()
  
  analyzation <- tbl(con, "openai_analyzation_corpus") %>% 
    select(-cf2, -af5) %>% 
    collect() %>%
    select(where(~ all(!is.na(.)))) %>% 
    aggregate_model_by_hash()
  
  data <- left_join(data_raw, analyzation, by = c("hash" = "hash")) %>%
    select(-text_numtokens) %>% 
    select(where(~ all(!is.na(.))))
  
  return(data)
}

consolidate_data_with_safety_facets <- function(table_name)
{
  data_raw <- tbl(con, table_name) %>% 
    collect()
  
  analyzation <- tbl(con, "openai_analyzation_corpus") %>% 
    collect() %>%
    select(where(~ all(!is.na(.)))) %>% 
    aggregate_model_by_hash()
  
  data <- left_join(data_raw, analyzation, by = c("hash" = "hash")) %>%
    select(-text_numtokens) %>% 
    select(where(~ all(!is.na(.))))
  
  return(data)
}





consolidate_data_joker <- function(table_name)
{
  data_raw <- tbl(con, table_name) %>% 
    collect()
  
  analyzation <- tbl(con, "openai_analyzation_corpus") %>% 
    collect() %>%
    select(where(~ all(!is.na(.)))) %>% 
    aggregate_model_by_hash()
  
  data <- left_join(data_raw, analyzation, by = c("hash" = "hash")) %>%
    select(-text_numtokens) %>% 
    select(where(~ all(!is.na(.))))
  
  return(data)
}
