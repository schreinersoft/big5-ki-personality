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


############## find differences
results <- data.frame()

# grafischer Vergleich
violins <- create_factor_violins(data_all, "author_name")
violins
ggsave(paste(graphics_output_folder, "/comparison_violins_factors.png", sep=""),
       plot = violins, dpi=300, width = 8, height = 6)

# ANOVA between Authors
o_diff <- create_stats_anova(data_all, "o_llm", "author_name", "O ANOVA") 
c_diff <- create_stats_anova(data_all, "c_llm", "author_name", "C ANOVA") 
e_diff <- create_stats_anova(data_all, "e_llm", "author_name", "E ANOVA") 
a_diff <- create_stats_anova(data_all, "a_llm", "author_name", "A ANOVA") 
n_diff <- create_stats_anova(data_all, "n_llm", "author_name", "N ANOVA") 
dfg <- o_diff$dfg
dfm <- o_diff$dfm
f_label <- paste("F(", dfg, ",", dfm, ")", sep="")
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>% 
  as.data.frame() %>% 
  select(-dfg, -dfm) %>% 
  mutate(across(everything(), ~ as.character(.))) %>%
  rename(!!f_label := F)
ft <- results %>% 
  flextable()
ft
save_as_docx(ft, path=paste(tables_output_folder, "/measure_anova.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_anova.xlsx",sep=""))

# U-Test
# ANOVA between Authors
o_diff <- create_stats_u_test(data_all, "o_llm", "author_name", "O U-Test") 
c_diff <- create_stats_u_test(data_all, "c_llm", "author_name", "C U-Test") 
e_diff <- create_stats_u_test(data_all, "e_llm", "author_name", "E U-Test") 
a_diff <- create_stats_u_test(data_all, "a_llm", "author_name", "A U-Test") 
n_diff <- create_stats_u_test(data_all, "n_llm", "author_name", "N U-Test") 
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>% 
  as.data.frame() %>% 
  mutate(across(everything(), ~ as.character(.)))
ft <- results %>% 
  flextable()
ft
save_as_docx(ft, path=paste(tables_output_folder, "/measure_u_test.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test.xlsx",sep=""))


















##########





woolf_aov_o <- data_all %>% 
  filter(author_name=="Virginia Woolf")
aov <- aov(n_llm ~ year, data=woolf_aov_o)
summary(aov)
residuals <- residuals(aov)
shapiro_test <- shapiro.test(residuals)

qqnorm(residuals)
qqline(residuals)


resid <-as.data.frame(aov$residuals)
