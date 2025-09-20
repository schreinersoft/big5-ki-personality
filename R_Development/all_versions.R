### Collector for all models, data and analyzations

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("transformation_functions.R")
source("output_folders.R")


fetch_raw_data <- function(table_name)
{
  data_raw <- tbl(con, table_name) %>% 
    select(-id, -updated_at, -input_tokens, -output_tokens) %>% 
    collect() %>% 
    select(where(~ !any(is.na(.))))
  return (data_raw)
}

analyze_all <- function(data, model_version)
{
  analyze_alpha_omega(data, model_version)
  analyze_factor_loadings(data, model_version)
  analyze_item_statistics(data, model_version)
}

create_all_graphics <- function(data, model_version)
{
  create_correlation_matrices(data, model_version)
  create_facet_densities(data, model_version)
  create_factor_densities(data, model_version)
  create_q_q_plot(data, model_version)
}





################################################# V1.0
model_version <- "v1.0"
data <- tbl(con, "openai_analyzation") %>% 
  select(-temperature) %>% 
  collect() %>% 
  rename(
    of1b = of1,
    of2b = of2,
    of3b = of3,
    cf1b = cf1,
    cf2b = cf2,
    cf3b = cf3,
    ef1b = ef1,
    ef2b = ef2,
    ef3b = ef3,
    af1b = af1,
    af2b = af2,
    af3b = af3,
    nf1b = nf1,
    nf2b = nf2,
    nf3b = nf3
  )
data_aggregated <- aggregate_model(data)
db_write_model(data_aggregated, model_version)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)
analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


################################################# V1.1
model_version <- "v1.1"
data_aggregated <- data %>% 
  select(-cf1b, -ef2b, -af3b, -nf3b) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)



################################################# V1.2
model_version <- "v1.2"
data_aggregated <- data %>% 
  select(-cf1b, -ef2b, -nf2b) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)


################################################# V2.0
model_version <- "v2.0"

# v2 in publication is v3 in data XXX
data <- tbl(con, "openai_analyzation_v3") %>% 
  select(-updated_at) %>%
  filter(essay_id <= 250) %>% 
  collect() %>% 
  drop_na("of1")

data_aggregated <- aggregate_model(data)

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)


################################################# V2.1
model_version <- "v2.1"

data_aggregated <- data %>% 
  select(-of3, -of4, -cf1, -ef1, -ef3, -af2, -nf2, -nf5) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


################################################# V2.2
model_version <- "v2.2"

data_aggregated <- data %>% 
  select(-of3, -of4, -cf1, -ef1, -ef3, -ef6, -af2, -nf2, -nf5) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)

################################################# V2.3
model_version <- "v2.3"

data_aggregated <- data %>% 
  select(-of3, -of4, -of6, -cf1, -cf4, -ef1, -ef3, -ef6, -af2, -af5, -nf2, -nf5) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)



################################################# V3.0
model_version <- "v3.0"

# DANGER !!! v2 in publication is v3 XXX
data <- tbl(con, "openai_analyzation_v2") %>% select(-updated_at) %>%
  collect() %>% 
  drop_na("of1")

data_aggregated <- data %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)


################################################# V4.0
model_version <- "v4.000"
data <- tbl(con, "google_analyzation") %>% select(-updated_at) %>%
  filter(essay_id <= 50) %>% 
  collect() %>% 
  drop_na("of1")

data_temp0 <- data %>% 
  filter(temperature == 0)

create_essay_histograms(data_temp0, model_version, 27)
create_essay_histograms(data_temp0, model_version, 42)
create_essay_histograms(data_temp0, model_version, 112)

data_aggregated <- data %>% 
  filter(temperature == 0) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)

model_version <- "v4.002"
data_aggregated <- data %>% 
  filter(temperature == 0.2) %>% 
  aggregate_model()
db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


model_version <- "v4.004"
data_aggregated <- data %>% 
  filter(temperature == 0.4) %>% 
  aggregate_model()
db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)


model_version <- "v4.006"
data_aggregated <- data %>% 
  filter(temperature == 0.6) %>% 
  aggregate_model()
db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


model_version <- "v4.008"
data_aggregated <- data %>% 
  filter(temperature == 0.8) %>% 
  aggregate_model()
db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


model_version <- "v4.010"
data_aggregated <- data %>% 
  filter(temperature == 1.0) %>% 
  aggregate_model()
db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)

data$temp <- as.factor(data$temperature)
## Grouped by temperature
all_factors <- data %>% select(
  starts_with("o_"),
  starts_with("c_"),
  starts_with("e_"),
  starts_with("a_"),
  starts_with("n_")) %>% names()
o_facets <- data %>% select(starts_with(("of"))) %>% names()
c_facets <- data %>% select(starts_with(("cf"))) %>% names()
e_facets <- data %>% select(starts_with(("ef"))) %>% names()
a_facets <- data %>% select(starts_with(("af"))) %>% names()
n_facets <- data %>% select(starts_with(("nf"))) %>% names()
all_facet_names <- c(names(o_facets), names(c_facets), names(e_facets), 
                     names(a_facets), names(n_facets))



data_temp <- data %>% 
  rowwise() %>% 
  mutate(
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
  )

data_temp_aggr <- data %>% 
  group_by(essay_id, temp) %>% 
  summarise(
    across(all_of(all_facet_names), ~ mean(.x, na.rm = TRUE)),
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
    .groups = "drop"
)


data_temp <- data_temp_aggr

model_oneway <- aov(o_llm ~ temp, data = data_temp)
summary(model_oneway)
model_oneway <- aov(c_llm ~ temp, data = data_temp)
summary(model_oneway)
model_oneway <- aov(e_llm ~ temp, data = data_temp)
summary(model_oneway)
model_oneway <- aov(a_llm ~ temp, data = data_temp)
summary(model_oneway)
model_oneway <- aov(n_llm ~ temp, data = data_temp)
summary(model_oneway)



data_temp %>% 
  ggplot(aes(x=o_llm, group=temp, fill=temp))+
  geom_boxplot()
data_temp %>% 
  ggplot(aes(x=c_llm, group=temp, fill=temp))+
  geom_boxplot()
data_temp %>% 
  ggplot(aes(x=e_llm, group=temp, fill=temp))+
  geom_boxplot()
data_temp %>% 
  ggplot(aes(x=a_llm, group=temp, fill=temp))+
  geom_boxplot()
data_temp %>% 
  ggplot(aes(x=n_llm, group=temp, fill=temp))+
  geom_boxplot()

# Speziell Analyse Temperatur Faktoren
plots <- list()
palette <- "Oranges"
alpha <- 0.4

plots[[1]] <- data_temp %>% 
  ggplot(aes(x = o_llm, group=temp, fill=temp)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = variable_names[["O"]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[2]] <- data_temp %>% 
  ggplot(aes(x = c_llm, group=temp, fill=temp)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = variable_names[["C"]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[3]] <- data_temp %>% 
  ggplot(aes(x = e_llm, group=temp, fill=temp)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = variable_names[["E"]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[4]] <- data_temp %>% 
  ggplot(aes(x = a_llm, group=temp, fill=temp)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = variable_names[["A"]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[5]] <- data_temp %>% 
  ggplot(aes(x = n_llm, group=temp, fill=temp)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = variable_names[["N"]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

library(cowplot)
# Legend only
temp_plot <- ggplot(data_temp, aes(fill = temp)) +
  geom_density(aes(x = n_llm), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = palette)+
  labs(title = "",
       x = "",
       y = "",
       fill = "Temperatur")
plots[[6]] <- get_legend(temp_plot)

combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] + plot_layout(ncol = 3)
combined_plot

ggsave(paste(graphics_output_folder, "/density_with_temperature_", model_version, ".png"), plot = combined_plot, dpi=300, width = 8, height = 5)



################################################# V4.1
model_version <- "v4.1"
data <- tbl(con, "google_analyzation") %>% select(-updated_at) %>%
  collect() %>% 
  drop_na("of1")

data_temp0 <- data %>% 
  filter(temperature == 0)

create_essay_histograms(data_temp0, model_version, 27)
create_essay_histograms(data_temp0, model_version, 42)
create_essay_histograms(data_temp0, model_version, 112)

data_aggregated <- data %>% 
  filter(temperature == 0) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)













################################################# V5.X
model_version <- "v5.X"
o_facets <- c("of3b", "of1", "of2", "of5")
c_facets <- c("cf2b", "cf3b", "cf3", "cf5")
e_facets <- c("ef2", "ef3b", "ef4", "ef5")
a_facets <- c("af1b", "af1", "af3", "af6")
n_facets <- c("nf1", "nf4", "nf6")
data_bfi <- tbl(con, "openai_analyzation") %>% 
  select(essay_id, of3, cf2, cf3, ef3, af1) %>% 
  collect() %>% 
  group_by(essay_id) %>%
  summarise(
    of3 = mean(of3, na.rm = TRUE),
    cf2 = mean(cf2, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    ef3 = mean(ef3, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    .groups = "drop") %>% 
  rename(of3b = of3,
         cf2b = cf2,
         cf3b = cf3,
         ef3b = ef3,
         af1b = af1,
         essay_idb = essay_id)

data_neo <- tbl(con, "openai_analyzation_v3") %>%
  select(essay_id, of1, of2, of5, cf3, cf5, ef2, ef4, ef5, af1, af3, af6, nf1, nf4, nf6) %>% 
  collect() %>% 
  group_by(essay_id) %>%
  summarise(
    of1 = mean(of1, na.rm = TRUE),
    of2 = mean(of2, na.rm = TRUE),
    of5 = mean(of5, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    cf5 = mean(cf5, na.rm = TRUE),
    ef2 = mean(ef2, na.rm = TRUE),
    ef4 = mean(ef4, na.rm = TRUE),
    ef5 = mean(ef5, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    af3 = mean(af3, na.rm = TRUE),
    af6 = mean(af6, na.rm = TRUE),
    nf1 = mean(nf1, na.rm = TRUE),
    nf4 = mean(nf4, na.rm = TRUE),
    nf6 = mean(nf6, na.rm = TRUE),
    .groups = "drop")
data_aggregated <- left_join(data_bfi, data_neo, by = c("essay_idb" = "essay_id")) %>%
  rowwise() %>% 
  mutate(
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE)
  ) %>% 
  rename(essay_id = essay_idb)

db_write_model(data_aggregated, model_version)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)

################################################# V5.0
model_version <- "v5.0"
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(-updated_at) %>%
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  filter(essay_id <= 250) %>% 
  collect()

pre <- data %>% 
  select(where(~ all(!is.na(.))))
o_facets <- pre %>% select(starts_with(("of")))
c_facets <- pre %>% select(starts_with(("cf")))
e_facets <- pre %>% select(starts_with(("ef")))
a_facets <- pre %>% select(starts_with(("af")))
n_facets <- pre %>% select(starts_with(("nf")))

all_facet_names <- c(names(o_facets), names(c_facets), names(e_facets), 
                     names(a_facets), names(n_facets))
summ <- data %>% 
  select(where(~ all(!is.na(.)))) %>% 
  group_by(essay_id) %>% 
  summarize(across(all_of(all_facet_names), ~ sd(.x, na.rm = TRUE)),
            .groups = "drop")



data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

db_write_model(data_aggregated, model_version)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


################################################# V5.0n
model_version <- "v5.0n"
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(-updated_at) %>%
  filter(model=="gpt-5-nano-2025-08-07") %>% 
  filter(essay_id <= 250) %>% 
  collect()

data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

db_write_model(data_aggregated, model_version)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)



################################################# V5.1
model_version <- "v5.1"
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(-updated_at) %>%
  select(-af1) %>% 
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  filter(essay_id <= 250) %>% 
  collect()

data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

db_write_model(data_aggregated, model_version)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


################################################# V5.1n
model_version <- "v5.1n"
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(-updated_at) %>%
  select(-af1) %>% 
  filter(model=="gpt-5-nano-2025-08-07") %>% 
  filter(essay_id <= 250) %>% 
  collect()

data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

db_write_model(data_aggregated, model_version)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)

analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)









#################### XXX corpus Korrelationskontrolle
corpus <- analyzation %>% 
aggregate_model_hash()

des <- psych::describe(corpus)
model_version <- "woolf"
data <- corpus

analyze_alpha_omega(data, model_version)
analyze_factor_loadings(data, model_version)
analyze_item_statistics(data, model_version)
create_correlation_matrices(data, model_version)
create_facet_densities(data, model_version)
create_factor_densities(data, model_version)
create_q_q_plot(data, model_version)
# -> sehr hohe Korrelationen!

woolf <- tbl(con, "woolf") %>% 
  collect()

data <- left_join(woolf, corpus, by = c("hash" = "hash")) %>% 
  select(-text_raw)

mintokens <- seq(0,500, 50)
for (mintoken in mintokens) {
  model_name <- paste(model_version, "_min_",mintoken, sep="")
  data_min <- data %>% 
    filter(as.integer(text_raw_numtokens) > as.integer(mintoken))
  
  #analyze_factor_loadings(data_min, model_name)
  create_correlation_matrices(data_min, model_name)
  create_facet_densities(data_min, model_name)
  create_factor_densities(data_min, model_name)
}

mintokens <-200
data_min <- data %>% 
  filter(as.integer(text_raw_numtokens) > as.integer(mintokens))
