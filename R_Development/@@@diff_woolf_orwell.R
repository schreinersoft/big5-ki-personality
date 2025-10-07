library(tidyverse)
library(corrr)

root_folder <- "C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen/measurement"

source("sources/connect_database.R")
source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders_measurement.R")
source("sources/difference_functions.R")
source("sources/combined_names_EN_DE.R")

corpus_name <- "woolf"
data_woolf_raw <- consolidate_data(corpus_name)
data_woolf <- data_woolf_raw %>% 
  filter(text_raw_numtokens >= 150) %>% 
  select(author_name, author_age, author_sex, year, month, text_type,
         o_llm, c_llm, e_llm, a_llm, n_llm)

corpus_name <- "orwell"
data_orwell_raw <- consolidate_data(corpus_name)
data_orwell <- data_orwell_raw %>% 
  filter(text_raw_numtokens >= 150) %>% 
  select(author_name, author_age, author_sex, year, month, text_type,
         o_llm, c_llm, e_llm, a_llm, n_llm)

data_all <- rbind(data_woolf, data_orwell)
rm(data_woolf_raw, data_orwell_raw, data_woolf, data_orwell)

dbWriteTable(con, "data_all", data_all, overwrite = TRUE, row.names = FALSE)

####################
data_all <- tbl(con, "data_all") %>% collect()


############## Unterschiede zwischen den Autoren
results <- data.frame()

# grafischer Vergleich
data_all_sorted <- data_all %>%
  mutate(author_name = factor(author_name, levels = c("Virginia Woolf", "George Orwell")))

violins <- create_factor_violins(data_all_sorted, "author_name")
violins
ggsave(paste(graphics_output_folder, "/comparison_violins_factors.jpg", sep=""),
       plot = violins, dpi=600, width = 7, height = 4)

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
save_as_docx(ft, path=paste(tables_output_folder, "/measure_anova_woolf_orwell.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_anova_woolf_orwell.xlsx",sep=""))

# U-Test between Auth
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
save_as_docx(ft, path=paste(tables_output_folder, "/measure_u_test_woolf_orwell.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test_woolf_orwell.xlsx",sep=""))



results <- data_all %>% 
  select(ends_with("llm")) %>% 
  group_by(author_name) %>%
  group_modify(~ psych::describe(.x) %>% 
                 as.data.frame() %>%
                 rownames_to_column("variable")) %>%
  ungroup()
  
  
ft <- results %>% 
  flextable()
ft
save_as_docx(ft, path=paste(tables_output_folder, "/measure_u_test_woolf_orwell.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test_woolf_orwell.xlsx",sep=""))












############# Messung der zeitlichen Konstanz
####################################################################### Woolf
author <- "Virginia Woolf"
agedata <- data_all %>%
  filter(author_name == author) %>% 
  mutate(author_age = as.factor(author_age))


# ANOVA between age
o_diff <- create_stats_anova_without_effectsizes(agedata, "o_llm", "author_age", "O ANOVA") 
c_diff <- create_stats_anova_without_effectsizes(agedata, "c_llm", "author_age", "C ANOVA") 
e_diff <- create_stats_anova_without_effectsizes(agedata, "e_llm", "author_age", "E ANOVA") 
a_diff <- create_stats_anova_without_effectsizes(agedata, "a_llm", "author_age", "A ANOVA") 
n_diff <- create_stats_anova_without_effectsizes(agedata, "n_llm", "author_age", "N ANOVA") 
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
save_as_docx(ft, path=paste(tables_output_folder, "/measure_anova_age_", author, "_.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_anova_age_", author, "_.xlsx",sep=""))


# U-Test between age
o_diff <- create_stats_h_test(agedata, "o_llm", "author_age", "O H-Test") 
c_diff <- create_stats_h_test(agedata, "c_llm", "author_age", "C H-Test") 
e_diff <- create_stats_h_test(agedata, "e_llm", "author_age", "E H-Test") 
a_diff <- create_stats_h_test(agedata, "a_llm", "author_age", "A H-Test") 
n_diff <- create_stats_h_test(agedata, "n_llm", "author_age", "N H-Test") 
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>% 
  as.data.frame() %>% 
  mutate(across(everything(), ~ as.character(.)))
ft <- results %>% 
  flextable()
ft
save_as_docx(ft, path=paste(tables_output_folder, "/measure_u_test_age_", author, "_.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test_age_", author, "_.xlsx",sep=""))


####################################################################### Orwell
author <- "George Orwell"
agedata <- data_all %>%
  filter(author_name == author) %>% 
  mutate(author_age = as.factor(author_age))


# ANOVA between age
o_diff <- create_stats_anova_without_effectsizes(agedata, "o_llm", "author_age", "O ANOVA") 
c_diff <- create_stats_anova_without_effectsizes(agedata, "c_llm", "author_age", "C ANOVA") 
e_diff <- create_stats_anova_without_effectsizes(agedata, "e_llm", "author_age", "E ANOVA") 
a_diff <- create_stats_anova_without_effectsizes(agedata, "a_llm", "author_age", "A ANOVA") 
n_diff <- create_stats_anova_without_effectsizes(agedata, "n_llm", "author_age", "N ANOVA") 
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
save_as_docx(ft, path=paste(tables_output_folder, "/measure_anova_age_", author, "_.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_anova_age_", author, "_.xlsx",sep=""))


# U-Test between age
o_diff <- create_stats_h_test(agedata, "o_llm", "author_age", "O H-Test") 
c_diff <- create_stats_h_test(agedata, "c_llm", "author_age", "C H-Test") 
e_diff <- create_stats_h_test(agedata, "e_llm", "author_age", "E H-Test") 
a_diff <- create_stats_h_test(agedata, "a_llm", "author_age", "A H-Test") 
n_diff <- create_stats_h_test(agedata, "n_llm", "author_age", "N H-Test") 
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>% 
  as.data.frame() %>% 
  mutate(across(everything(), ~ as.character(.)))
ft <- results %>% 
  flextable()
ft
save_as_docx(ft, path=paste(tables_output_folder, "/measure_u_test_age_", author, "_.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test_age_", author, "_.xlsx",sep=""))













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
