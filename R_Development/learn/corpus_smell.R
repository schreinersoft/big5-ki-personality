library(tidyverse)
source("sources/connect_database.R")
source("sources/transformation_functions.R")


analyzation <- tbl(con, "openai_analyzation_corpus") %>% 
  select(-cf2, -af5, -nf3) %>% 
  collect() %>%
  select(where(~ all(!is.na(.))))


stats <- analyzation %>%
  group_by(hash) %>% 
  group_modify(~ describe(select(.x, starts_with("ef"))) %>%
                 rownames_to_column("variable")) %>%
  ungroup()

mean(stats$sd, na.rm=TRUE)