library(tidyverse)
library(readr)

essays <- read_csv("raw/essays-dataset-raw_utf8.csv", 
                   locale = locale(),
                   col_names = c("author", "text", "yne", "ynn", "yna", "ync", "yno"),
                   skip = 1)

yn <- function(x){
  ifelse(x == "y", 1, 0)
}

# create binary variables in nice ocean order
essays$o_binary = yn(essays$yno)
essays$c_binary = yn(essays$ync)
essays$e_binary = yn(essays$yne)
essays$a_binary = yn(essays$yna)
essays$n_binary = yn(essays$ynn)

# drop old columns, add id and sort table
essays <- essays %>% 
  select(-c(yno, ync, yne, yna, ynn)) %>%
  mutate(id = row_number()) %>% 
  relocate(id, .before=author)

# refresh data in table
dbExecute(con, "DELETE FROM essays")
dbWriteTable(con, "essays", essays, append = TRUE,row.names = FALSE)

essays %>% write_csv("00_cleaned/essays_cleaned.csv")



