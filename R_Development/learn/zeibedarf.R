source("sources/connect_database.R")

analyzation <- tbl(con, "openai_analyzation_corpus")


lens <- analyzation %>% 
  select(started_at, finished_at) %>% 
  collect()


lens_times <- lens %>%
  mutate(elapsed = finished_at-started_at-2)

mean(lens_times$elapsed) * 60 * 60
