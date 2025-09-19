library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("output_folders.R")


woolf <- tbl(con, "woolf") %>% 
  select(hash, author_age, year, month, text_raw_numtokens, text_numtokens) %>% 
  collect()



## Datensatzexploration
stats <- data %>% 
  group_by(year, month) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(year, month)

woolf %>% filter(text_raw_numtokens < 100) %>% 
  ggplot(aes(x=text_raw_numtokens)) +
  geom_histogram()

stats %>% 
  ggplot(aes(x=year, y=n)) +
  geom_col() +
  theme_minimal()

data %>% 
  ggplot(aes(x=author_age, y=text_raw_numtokens)) +
  geom_point()+
  theme_minimal()


