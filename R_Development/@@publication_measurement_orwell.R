library(tidyverse)
library(corrr)
library(psych)
library(writexl)
library(scales)

root_folder <- "C:/Users/bernd/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen/measurement"

source("sources/connect_database.R")
source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders.R")
source("sources/combined_names_EN_DE.R")

corpus_name <- "orwell"
data <- consolidate_data(corpus_name)

birth_year <- 1903
birth_month <- 6

data <- data %>% 
   mutate(author_age = round((((year*12)+month - ((birth_year*12)+birth_month+1)) / 12),0))


factors <- data %>% select(ends_with("llm")) %>% names()


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

stats <- data %>%
  group_by(author_age) %>%
  group_modify(~ psych::describe(select(.x, ends_with("_llm"))) %>%
  as.data.frame() %>% 
  rownames_to_column("variable")) %>%
  arrange(author_age, variable) %>% 
  ungroup()
ft <- stats %>% 
  as.data.frame() %>% 
  flextable()
ft
save_as_docx(ft, path = paste(tables_output_folder, "/desc_age_", corpus_name, ".docx", sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/desc_age_", corpus_name, ".xlsx",sep=""))


stats_monthly <- data %>%
  mutate(author_age_months = (author_age * 12)+month) %>% 
  group_by(author_age_months) %>%
  group_modify(~ psych::describe(select(.x, ends_with("_llm"))) %>%
                 as.data.frame() %>% 
                 rownames_to_column("variable")) %>%
  arrange(variable, author_age_months) %>% 
  ungroup()



# correlations
data %>% 
  select(all_of(factors)) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  flextable()

corr_stats <- data %>% 
  select(all_of(factors)) %>% 
  corr.test() 
  
sink(paste(stats_output_folder, "/corr_", corpus_name, ".txt", sep=""))
round(corr_stats$r, 2)
round(corr_stats$p, 3)
sink()

# GRAPHICS
create_factor_densities(data, corpus_name)


# Lebensereignisse definieren
ereignisse <- data.frame(
  author_age = 
    c(30,
      34,
      39,
      42,
      44
      ),
  ereignis = 
    c("Erstes Buch",
      "Bürgerkrieg",
      "Kündigung BBC",
      "Tod Ehefrau",
      "Schottland"
      )
)



### trendlines jährlich
min_max <- stats %>% 
  filter(author_age >28)
min_age <- min(min_max$author_age)
max_age <- max(min_max$author_age)

trendlines <- stats %>% 
  filter(author_age >28) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ variable, scales = "fixed", labeller = labeller(variable = variable_names)) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "solid", alpha = 0.2, se = TRUE, size = 0.6, fill="darkgrey") +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age, max_age), breaks = breaks_width(2)) +
  labs(
    title = "",
    x = "Alter",
    y = "M (Skalenwert)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  
trendlines
ggsave(paste(graphics_output_folder,"/lines_with_trend_", corpus_name, ".jpg", sep = ""), 
       plot = trendlines, dpi = 600, width = 7, height = 4)


thinner_trendlines <- stats %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 0.5, linejoin="round") +
  geom_point(size=0.7) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ variable, scales = "fixed", labeller = labeller(variable = variable_names)) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dotted", alpha = 0.2, se = TRUE, size = 0.3, fill="darkgrey") +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age, max_age), breaks = breaks_width(2)) +
  labs(
    title = "",
    x = "Alter",
    y = "Skalenwert"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  
thinner_trendlines
ggsave(paste(graphics_output_folder,"/thinner_lines_with_trend_", corpus_name, ".jpg", sep = ""), 
       plot = thinner_trendlines, dpi = 600, width = 7, height = 4)


# alle zusammen
trendlines_flat <- stats %>% 
  filter(author_age >28) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  scale_color_brewer(palette = "Set1", labels = variable_names) +
  scale_fill_brewer(palette = "Set1") +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dashed", alpha = 0.2, se = TRUE, size = 0.2, fill="darkgrey") +
  geom_vline(data = ereignisse, 
             aes(xintercept = author_age), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.7) +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age, max_age), breaks = breaks_width(2)) +
  geom_text(data = ereignisse,
            aes(x = author_age, y = 3, label = ereignis),
            angle = 0, vjust = -0.3, hjust = 1.05,
            size = 3, color = "red",
            inherit.aes = FALSE) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M (Skalenwert)",
    color = ""
  ) +
  theme_minimal() #+
#theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines_flat
ggsave(paste(graphics_output_folder,"/lines_flat_", corpus_name, ".jpg", sep = ""), 
       plot = trendlines_flat, dpi = 600, width = 8, height = 4)





# alle zusammen
thinner_trendlines_flat <- stats %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  scale_color_brewer(palette = "Set1", labels = variable_names) +
  scale_fill_brewer(palette = "Set1") +
  #geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 0.5, linejoin="round") +
  geom_point(size=0.8) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dotted", alpha = 0.4, se = TRUE, size = 0.4) +
  geom_vline(data = ereignisse, 
             aes(xintercept = author_age), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.7) +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age-0.5, max_age), breaks = breaks_width(2)) +
  geom_text(data = ereignisse,
            aes(x = author_age, y = 3, label = ereignis),
            angle = 0, vjust = -0.3, hjust = 1.05,
            size = 3, color = "red",
            inherit.aes = FALSE) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "Skalenwert",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")


thinner_trendlines_flat
ggsave(paste(graphics_output_folder,"/thinner_lines_flat_", corpus_name, ".jpg", sep = ""), 
       plot = thinner_trendlines_flat, dpi = 300, width = 7, height = 5)









############################################ MONTHLY
thinner_trendlines_flat_monthly <- stats_monthly %>% 
  filter(author_age_months >= 52*12) %>% 
  filter(author_age_months <= 56*12) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age_months, y = mean, color = variable, fill = variable)) +
  scale_color_brewer(palette = "Set1", labels = variable_names) +
  scale_fill_brewer(palette = "Set1") +
  #geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 0.5, linejoin="round") +
  geom_point(size=0.8) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dotted", alpha = 0.4, se = TRUE, size = 0.4) +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age-0.5, max_age), breaks = breaks_width(2)) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "Skalenwert",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")


thinner_trendlines_flat_monthly
ggsave(paste(graphics_output_folder,"/thinner_lines_flat_", corpus_name, ".jpg", sep = ""), 
       plot = thinner_trendlines_flat, dpi = 300, width = 7, height = 5)

thinner_trendlines_flat_monthly <- stats_monthly %>% 
  filter(author_age_months >= 52*12) %>% 
  filter(author_age_months <= 56*12)






























trendlines_annotations <- stats %>% 
  filter(author_age >34) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dashed", alpha = 0.2, se = TRUE, size = 0.2, fill="darkgrey") +
  geom_vline(data = ereignisse, 
             aes(xintercept = author_age), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.7) +
  scale_y_continuous(limits = c(3.8, 8), breaks = 1:9) +
  scale_x_continuous() +
  scale_color_discrete(name="", labels=variable_names) +
  geom_text(data = ereignisse,
            aes(x = author_age, y = 8, label = ereignis),
            angle = 0, vjust = -0.3, hjust = 1,
            size = 3, color = "red",
            inherit.aes = FALSE) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M"
  ) +
  theme_minimal() #+
#theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines_annotations
ggsave(paste(graphics_output_folder,"/lines_flat_annotation_", corpus_name, ".jpg", sep = ""), 
       plot = trendlines_annotations, dpi = 600, width = 6, height = 4)




################################################# monatlich
# alle zusammen  --  nun ja...
trendlines_flat_monthly <- stats_monthly %>% 
  filter(author_age_months > 450) %>%
  filter(author_age_months < 550) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm"))) %>% 
  ggplot(aes(x = author_age_months, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin="round") +
  geom_point(size=1.2) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dashed", alpha = 0.2, se = TRUE, size = 0.2, fill="darkgrey") +
  scale_y_continuous(limits = c(3.8, 8), breaks = 1:9) +
  scale_color_discrete(name="", labels=variable_names) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter (Monate)",
    y = "M"
  ) +
  theme_minimal() #+
#theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines_flat_monthly
ggsave(paste(graphics_output_folder,"/lines_flat_monthly_", corpus_name, ".jpg", sep = ""), 
       plot = trendlines_flat_monthly, dpi = 600, width = 6, height = 4)



########## detrending
loess_model <- loess(o_llm ~ author_age, data=data, span=0.5)
data$trend <- predict(loess_model)
data$residuals <- data$o_llm - data$trend
data$detrended <- data$residuals + mean(data$o_llm)

data %>% 
  summarise(
    across()
  )
  ggplot(aes(x=year, y=detrended))+
  geom_line()+
  theme_minimal()
describe(data)




### BOXPLOTS
data %>% 
  pivot_longer(all_of(factors), names_to = "factor", values_to="value") %>% 
  ggplot(aes(x=factor, y=value)) +
  geom_boxplot() +
  theme_minimal()

###### Einzele Einträge suchen


e_low <- data %>% 
  filter(e_llm < 2.4)

c_high <- data %>% 
  filter(c_llm > 7.9)



