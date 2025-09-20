library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("output_folders.R")


dataset_exploration <- tbl(con, "woolf") %>% 
  select(hash, author_age, year, month, text_raw_numtokens, text_numtokens) %>% 
  collect()


## Datensatzexploration
# Anzahl nach Zeit
y_m_stats <- dataset_exploration %>% 
  group_by(year, month) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(year, month)

y_stats <- dataset_exploration %>% 
  group_by(author_age) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(author_age)
describe(y_stats)
describe(y_stats$n)

dataset_exploration %>% filter(text_raw_numtokens < 150)





# Anzahl nach Tokens
describe(dataset_exploration$text_raw_numtokens, na.rm=TRUE)
dataset_exploration %>% 
  ggplot(aes(x=text_raw_numtokens)) +
  geom_histogram()

mintokens = seq(0,1000, 50)
for (mint in mintokens) {
  dataset_min <- dataset_exploration %>% 
    filter(text_raw_numtokens > mint)
  print(paste(mint, ":", as.character(count(dataset_min)$n)))
}

stats %>% 
  ggplot(aes(x=year, y=n)) +
  geom_col() +
  theme_minimal()

dataset_exploration %>% 
  ggplot(aes(x=author_age, y=text_raw_numtokens)) +
  geom_point()+
  theme_minimal()


