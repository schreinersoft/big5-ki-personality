library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("output_folders.R")

# calculate all means of facets and factors
aggregate_model_by_hash <- function(d)
{
  o_facets <- d %>% select(starts_with(("of"))) %>% names()
  c_facets <- d %>% select(starts_with(("cf"))) %>% names()
  e_facets <- d %>% select(starts_with(("ef"))) %>% names()
  a_facets <- d %>% select(starts_with(("af"))) %>% names()
  n_facets <- d %>% select(starts_with(("nf"))) %>% names()
  
  all_facet_names <- c(o_facets,c_facets, e_facets, 
                       a_facets, n_facets)
  
  # Aggregate the data to obtain 1 measurement per hash
  da <- d %>%
    select(hash, all_of(all_facet_names)) %>% 
    group_by(hash) %>%
    summarise(
      across(all_of(all_facet_names), ~ mean(.x, na.rm = TRUE)),
      o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
      c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
      e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
      a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
      n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
      .groups = "drop") %>% 
    select(hash, all_of(ends_with("_llm")))
  
  return(da)
}


woolf <- tbl(con, "woolf") %>% 
  select(hash, year, month, text_raw_numtokens, text_numtokens) %>% 
  collect()

sum(woolf$text_raw_numtokens)

woolf_analyzation <- tbl(con, "openai_analyzation_corpus") %>% 
  collect() %>%
  select(where(~ all(!is.na(.)))) %>% 
  aggregate_model_by_hash()

data <- right_join(woolf, woolf_analyzation, by = c("hash" = "hash")) %>%
  drop_na(o_llm)


create_factor_densities(data, "woolf")

desc.stats <- data %>% 
  select(all_of(ends_with("_llm"))) %>% 
  psych::describe()
print(desc.stats)


stats <- joined %>% 
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







# XXX REMOVE TOO SMALL ITEMS !?

woolf %>% filter(text_numtokens < 200) %>% 
  ggplot(aes(x=text_numtokens)) +
  geom_histogram()
