library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders.R")

table_name <- "noise"

data <- consolidate_data(table_name)

factors <- data %>% select(ends_with("llm")) %>% names()


# also possible: group_by(year, month) %>%
# DESCRIPTIVES
stats_z <- data %>%
  mutate(across(ends_with("_llm"), ~ as.numeric(scale(.x)))) %>% 
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
                 rownames_to_column("variable")) %>%
  ungroup()


# correlations
data %>% 
  select(all_of(factors)) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  flextable()

mean(stats_z$se)

# GRAPHICS
create_factor_densities_z(data, table_name)

