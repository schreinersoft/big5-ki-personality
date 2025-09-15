library(tidyverse)

source("connect_database.R")

woolf_db <- tbl(con, "woolf") %>% 
  select(hash, text, year, month) %>% 
  collect()

stats <- woolf_db %>% 
  group_by(year, month) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(year, month)

sum(stats$n)

stats %>% 
  ggplot(aes(x=year, y=n)) +
  geom_col() +
  theme_minimal()

summary(stats)  

