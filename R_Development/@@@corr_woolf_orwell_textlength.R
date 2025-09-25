library(tidyverse)
library(corrr)

root_folder <- "C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen/measurement"

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders_measurement.R")
source("difference_functions.R")
source("combined_names_EN.R")

corpus_name <- "woolf"
data_woolf_raw <- consolidate_data(corpus_name)
data_woolf <- data_woolf_raw %>% 
  filter(text_raw_numtokens >= 150) %>% 
  select(author_name, text_raw_numtokens,
         o_llm, c_llm, e_llm, a_llm, n_llm)

corpus_name <- "orwell"
data_orwell_raw <- consolidate_data(corpus_name)
data_orwell <- data_orwell_raw %>% 
  filter(text_raw_numtokens >= 150) %>% 
  select(author_name, text_raw_numtokens,
         o_llm, c_llm, e_llm, a_llm, n_llm)

data_all <- rbind(data_woolf, data_orwell)
rm(data_woolf_raw, data_orwell_raw, data_woolf, data_orwell)

dbWriteTable(con, "data_all", data_all, overwrite = TRUE, row.names = FALSE)

####################
data_all <- tbl(con, "data_all") %>% collect()


############## Unterschiede zwischen den Autoren
results <- data.frame()

##################### Woolf
data_test <- data_woolf %>%
  select(starts_with(("o_")),
         starts_with(("c_")),
         starts_with(("e_")),
         starts_with(("a_")),
         starts_with(("n_")),
         text_raw_numtokens)

sink(paste(stats_output_folder, "/output_analyzation_", model_version, ".txt", sep=""))

cor_matrix <- corr.test(data_test, use = "complete.obs", method="spearman")
cor_matrix_rounded <- round(cor_matrix$r, 2)
print("Correlation Matrix (Spearmans's rho):")
print(cor_matrix_rounded)
print("p-Values:")
print(round(cor_matrix$p, 3))
  
 
##################### Woolf
data_test <- data_orwell %>%
  select(starts_with(("o_")),
         starts_with(("c_")),
         starts_with(("e_")),
         starts_with(("a_")),
         starts_with(("n_")),
         text_raw_numtokens)

cor_matrix <- corr.test(data_test, use = "complete.obs", method="spearman")
cor_matrix_rounded <- round(cor_matrix$r, 2)
print("Correlation Matrix (Spearmans's rho):")
print(cor_matrix_rounded)
print("p-Values:")
print(round(cor_matrix$p, 3))

cor_matrix_rounded %>% 
  as.data.frame() %>%
  flextable()

round(cor_matrix$p, 3) %>% 
  as.data.frame() %>%
  flextable()

