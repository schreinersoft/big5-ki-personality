library(tidyverse)
library(corrr)

root_folder <- "C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen/measurement"


source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders_measurement.R")
source("factor-names-EN.R")

corpus_name <- "woolf"
data_woolf_raw <- consolidate_data(corpus_name)
data_woolf <- data_woolf_raw %>% 
  select(author_name, author_age, author_sex, year, month, text_type,
         o_llm, c_llm, e_llm, a_llm, n_llm)

corpus_name <- "orwell"
data_orwell_raw <- consolidate_data(corpus_name)
data_orwell <- data_orwell_raw %>% 
  select(author_name, author_age, author_sex, year, month, text_type,
         o_llm, c_llm, e_llm, a_llm, n_llm)

data_all <- rbind(data_woolf, data_orwell)
rm(data_woolf_raw, data_orwell_raw, data_woolf, data_orwell)

dbWriteTable(con, "data_all", data_all, overwrite = TRUE, row.names = FALSE)

####################
data_all <- tbl(con, "data_all") %>% collect()


woolf_aov_o <- data_all %>% 
  filter(author_name=="Virginia Woolf")
aov <- aov(n_llm ~ year, data=woolf_aov_o)
summary(aov)
residuals <- residuals(aov)
shapiro_test <- shapiro.test(residuals)

qqnorm(residuals)
qqline(residuals)


resid <-as.data.frame(aov$residuals)
