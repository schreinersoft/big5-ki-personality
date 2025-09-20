library(tidyverse)

source("connect_database.R")

korpora = c("woolf", "orwell", "benjamin")

for (k in korpora) {
  korpus <- wang <- tbl(con, k) %>% 
    select(text_raw_numtokens) %>% 
    collect()
  print(k)
  print(describe(korpus))
}
