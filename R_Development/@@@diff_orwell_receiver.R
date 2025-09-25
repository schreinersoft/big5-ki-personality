library(tidyverse)
library(corrr)
library(writexl)

root_folder <- "C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen/measurement"

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders_measurement.R")
source("difference_functions.R")
source("combined_names_EN.R")

corpus_name <- "orwell"
data_orwell_raw <- consolidate_data(corpus_name)
data_orwell <- data_orwell_raw %>% 
  filter(text_raw_numtokens >= 150) %>% 
  select(author_name, author_age, receiver_sex,
         o_llm, c_llm, e_llm, a_llm, n_llm)

data_all <- data_orwell

############## Unterschiede zwischen den Autoren
results <- data.frame()

# grafischer Vergleich
violins <- create_factor_violins(data_all, "receiver_sex")
violins
ggsave(paste(graphics_output_folder, "/comparison_violins_orwell_receivers.png", sep=""),
       plot = violins, dpi=300, width = 8, height = 6)

data_all %>% 
  group_by(receiver_sex) %>% 
  summarize(n = n())

data_all <- data_all %>% mutate(receiver_sex = as.factor(receiver_sex))


# ANOVA between Receiver sexes
o_diff <- create_stats_anova(data_all, "o_llm", "receiver_sex", "O ANOVA") 
c_diff <- create_stats_anova(data_all, "c_llm", "receiver_sex", "C ANOVA") 
e_diff <- create_stats_anova(data_all, "e_llm", "receiver_sex", "E ANOVA") 
a_diff <- create_stats_anova(data_all, "a_llm", "receiver_sex", "A ANOVA") 
n_diff <- create_stats_anova(data_all, "n_llm", "receiver_sex", "N ANOVA") 
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
save_as_docx(ft, path=paste(tables_output_folder, "/measure_anova_orwell_receivers.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_anova_orwell_receivers.xlsx",sep=""))

# U-Test between Auth
o_diff <- create_stats_u_test(data_all, "o_llm", "receiver_sex", "O U-Test") 
c_diff <- create_stats_u_test(data_all, "c_llm", "receiver_sex", "C U-Test") 
e_diff <- create_stats_u_test(data_all, "e_llm", "receiver_sex", "E U-Test") 
a_diff <- create_stats_u_test(data_all, "a_llm", "receiver_sex", "A U-Test") 
n_diff <- create_stats_u_test(data_all, "n_llm", "receiver_sex", "N U-Test") 
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>% 
  as.data.frame() %>% 
  mutate(across(everything(), ~ as.character(.)))
ft <- results %>% 
  flextable()
ft
save_as_docx(ft, path=paste(tables_output_folder, "/measure_u_test_orwell_receivers.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test_orwell_receivers.xlsx",sep=""))


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
