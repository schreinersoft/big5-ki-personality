library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders.R")
source("factor-names-EN.R")


table_name <- "woolf"

data <- consolidate_data(table_name)

factors <- data %>% select(ends_with("llm")) %>% names()


# also possible: group_by(year, month) %>%
# DESCRIPTIVES
stats <- data %>%
  group_by(year) %>%
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
                 rownames_to_column("variable")) %>%
  arrange(variable, year) %>% 
  ungroup()


# correlations
data %>% 
  select(all_of(factors)) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  flextable()

mean(stats$se)

# GRAPHICS
create_factor_densities(data, table_name)

trendlines <- stats %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = year, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.5) +
  facet_wrap(~ variable, scales = "fixed", labeller = labeller(variable = factor_names)) +
  geom_smooth(method = "loess", linetype = "solid", alpha = 0.8, se = FALSE, size = 0.6, color="red") +
  scale_y_continuous(limits = c(4, 8), breaks = 1:9) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Messungen mit Trendlinie",
    x = "Jahr",
    y = "M"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines
ggsave(paste(graphics_output_folder,"/lines_with_trend_", table_name, ".png", sep = ""), 
       plot = trendlines, dpi = 600, width = 6, height = 4)

yearseq <- seq(1917, 1941)

for (y in yearseq) {
  name <- paste(table_name, "_", y, sep="")
  data_year <- data %>% 
    filter(as.integer(year) == as.integer(y))
  create_factor_densities(data_year, name)
}




######  nf3 mit rein
table_name <- "woolf"

data_nf3 <- consolidate_data_with_nf3(table_name)

# correlations
data_nf3 %>% 
  select(all_of(factors)) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  flextable()






















# über 150 token -> keine starke veränderung der linien
stats <- data %>%
  filter(text_raw_numtokens > 100) %>% 
  group_by(year) %>%
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
                 rownames_to_column("variable")) %>%
  arrange(variable, year) %>% 
  ungroup()

trendlines <- stats %>% 
  filter(year > 1917) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = year, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.0, linejoin="round") +
  geom_point(size=1.5) +
  facet_wrap(~ variable, scales = "fixed", labeller = labeller(variable = factor_names)) +
  geom_smooth(method = "loess", linetype = "solid", alpha = 0.8, se = FALSE, size = 0.6, color="red") +
  scale_y_continuous(limits = c(4, 8), breaks = 1:9) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Messungen mit Trendlinie",
    x = "Jahr",
    y = "M"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines
ggsave(paste(graphics_output_folder,"/lines_with_trend_min_150_", table_name, ".png", sep = ""), 
       plot = trendlines, dpi = 600, width = 6, height = 4)



