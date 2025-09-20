library(tidyverse)

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("measurement_functions.R")
source("output_folders_measurement.R")
source("factor-names-EN.R")


table_name <- "woolf"
data <- consolidate_data(table_name)

table_name <- paste(table_name, "_min_150", sep="")
data <- data %>% 
  filter(text_raw_numtokens >= 150)

factors <- data %>% select(ends_with("llm")) %>% names()


# also possible: group_by(year, month) %>%
# DESCRIPTIVES
stats <- data %>%
  group_by(author_age) %>%
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
                 rownames_to_column("variable")) %>%
  arrange(variable, author_age) %>% 
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
  filter(author_age >34) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  facet_wrap(~ variable, scales = "fixed", labeller = labeller(variable = factor_names)) +
  geom_smooth(method = "loess", linetype = "solid", alpha = 0.2, se = TRUE, size = 0.6, fill="darkgrey") +
  scale_y_continuous(limits = c(3.8, 8), breaks = 1:9) +
  labs(
    title = "",
    x = "Alter",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")  
trendlines
ggsave(paste(graphics_output_folder,"/lines_with_trend_", table_name, ".jpg", sep = ""), 
       plot = trendlines, dpi = 600, width = 6, height = 4)



trendlines_flat <- stats %>% 
  filter(author_age >34) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  geom_smooth(method = "loess", linetype = "dashed", alpha = 0.2, se = TRUE, size = 0.2, fill="darkgrey") +
  scale_y_continuous(limits = c(3.8, 8), breaks = 1:9) +
  scale_color_discrete(name="", labels=variable_names) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M"
  ) +
  theme_minimal() #+
  #theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines_flat
ggsave(paste(graphics_output_folder,"/lines_flat_", table_name, ".jpg", sep = ""), 
       plot = trendlines_flat, dpi = 600, width = 6, height = 4)



###### Einzele Einträge suchen
e_low <- data %>% 
  filter(a_llm < 4, author_age > 57)

n_high <- data %>% 
  filter(n_llm > 8.95)

o_high <- data %>% 
  filter(o_llm > 8.5)





################### KEIN EFFEKT!
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

# alle mit rein
data_all <- consolidate_data_with_safety_facets("woolf")

# correlations
data_all %>% 
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



