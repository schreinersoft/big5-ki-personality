library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders_measurement.R")

corpus_name <- "noise"

data <- consolidate_data(corpus_name)

factors <- data %>% select(ends_with("llm")) %>% names()

model <- data %>% 
  rename(essay_id = id)

db_write_model(model, "noise")

# also possible: group_by(year, month) %>%
# DESCRIPTIVES
desc <- data %>% 
  select(all_of(factors)) %>% 
  describe() %>% 
  as.data.frame() %>% 
  round(3)
ft <- desc %>% 
  flextable()
ft
save_as_docx(ft, path = paste(tables_output_folder, "/desc_", corpus_name, ".docx", sep=""))
mean(desc$se)

mean(stats_z$se)

corr_stats <- data %>% 
  select(all_of(factors)) %>% 
  corr.test() 

sink(paste(stats_output_folder, "/corr_", corpus_name, ".txt", sep=""))
round(corr_stats$r, 2)
round(corr_stats$p, 3)
sink()



# GRAPHICS
create_factor_densities(data, table_name)

### create densities


