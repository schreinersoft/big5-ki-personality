library(tidyverse)
source("sources/connect_database.R")

openai_analyzation_corpus <- tbl(con, "openai_analyzation_corpus") %>% 
  select(-updated_at, -finished_at, -started_at) %>% 
  collect() %>% 
  select(where(~ all(!is.na(.))))

dbWriteTable(con, "measurement_complete", openai_analyzation_corpus, overwrite = TRUE, row.names = FALSE)

