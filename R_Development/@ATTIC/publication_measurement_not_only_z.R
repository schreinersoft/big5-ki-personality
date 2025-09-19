library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("output_folders.R")

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

stats_z <- data %>%
  mutate(across(ends_with("_llm"), ~ as.numeric(scale(.x)))) %>% 
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



# eventuell tokens limit erhöhen
stats_330 <- data %>%
  filter(text_raw_numtokens > 330) %>% 
  group_by(year) %>%
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
                 rownames_to_column("variable")) %>%
  arrange(variable, year) %>% 
  ungroup()

data %>% 
  filter(text_raw_numtokens > 330) %>% 
  select(all_of(factors)) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  flextable()

# GRAPHICS
create_factor_densities(data, "woolf")

stats %>% 
  ggplot(aes(x = year, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  facet_wrap(~ variable, scales = "fixed") +
  geom_smooth(method = "loess", linetype = "dashed", alpha = 0.8, se = FALSE, size = 0.6, color="red") +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  labs(
    title = "LLM Measurements with Standard Error",
    x = "Jahr",
    y = "M"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since facets show the variables

stats_330 %>% 
  ggplot(aes(x = year, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0) +
  geom_smooth(method = "loess", linetype = "dashed", alpha = 0.8, se = FALSE, size = 0.6, color="red") +
  facet_wrap(~ variable, scales = "fixed") +
  scale_y_continuous(limits = c(4, 8), breaks = 1:9) +
  labs(
    title = "LLM Measurements with Standard Error",
    x = "Jahr",
    y = "M"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since facets show the variables



# z-normalisiert
trendlines <- stats_z %>% 
  filter(year > 1916) %>% 
  ggplot(aes(x = year, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=2.5) +
  facet_wrap(~ variable, scales = "fixed") +
  geom_smooth(method = "loess", linetype = "dashed", alpha = 0.8, se = FALSE, size = 0.6, color="red") +
  scale_y_continuous(limits = c(-2.0, 2.0), breaks = seq(-1.5, 1.5, 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Messungen mit Trendlinie",
    x = "Jahr",
    y = "M"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines
ggsave(paste(graphics_output_folder,"/lines_with_trend", table_name, ".png", sep = ""), 
       plot = trendlines, dpi = 300, width = 8, height = 6)
