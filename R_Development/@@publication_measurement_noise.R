library(tidyverse)

root_folder <- "C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen/measurement"

source("sources/connect_database.R")
source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders_measurement.R")

corpus_name <- "noise"

data <- consolidate_data(corpus_name)

gen_years <- function(from, to, total) {
  years <- seq(from, to)
  repeats <- total %/% length(years)  # Integer division
  result <- rep(years, each = repeats)
}

years_noise <- c(gen_years(20,50,250), 50, 50)
data <- data %>% 
  mutate(author_age = years_noise)
factor_names <- data %>% select(ends_with("llm")) %>% names()

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

stats <- data %>%
  group_by(author_age) %>%
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
                 rownames_to_column("variable")) %>%
  arrange(variable, author_age) %>% 
  ungroup()
ft <- stats %>% 
  as.data.frame() %>% 
  flextable()
ft
save_as_docx(ft, path = paste(tables_output_folder, "/desc_age_", corpus_name, ".docx", sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/desc_age_", corpus_name, ".xlsx",sep=""))

# GRAPHICS
create_factor_densities(data, table_name)




trendlines <- stats %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.0, linejoin="round") +
  geom_point(size = 1.2) +
  facet_wrap(~ variable, scales = "fixed", labeller = labeller(variable = factor_names)) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "solid", alpha = 0.2, se = TRUE, size = 0.6, fill="darkgrey") +
  scale_y_continuous(limits = c(2,8), breaks = 1:9) +
  labs(
    title = "",
    x = "Alter",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")  
trendlines
ggsave(paste(graphics_output_folder,"/lines_with_trend_", corpus_name, ".jpg", sep = ""), 
       plot = trendlines, dpi = 600, width = 6, height = 4)


# alle zusammen
trendlines_flat <- stats %>% 
  filter(author_age > 20) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dashed", alpha = 0.2, se = TRUE, size = 0.2, fill="darkgrey") +
  scale_y_continuous(limits = c(2, 8), breaks = 1:9) +
  scale_color_discrete(name="", labels=variable_names) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M"
  ) +
  theme_minimal() #+
#theme(legend.position = "none")
trendlines_flat
ggsave(paste(graphics_output_folder,"/lines_flat_", corpus_name, ".jpg", sep = ""), 
       plot = trendlines_flat, dpi = 600, width = 6, height = 4)
