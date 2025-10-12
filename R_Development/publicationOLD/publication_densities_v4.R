# Model version for printing
measurement_version <- "v4.0"

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/NEO-PI-R-Names-EN.R")
source("sources/Factor-Names-EN.R")

# get data
data <- tbl(con, "google_analyzation") %>% select(-updated_at) %>%
  filter(essay_id <= 50) %>% 
  collect() %>% 
  drop_na("of1")

# set up facets
o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
#all_factors <- c("O", "C", "E", "A", "N")
all_factors <- c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
all_factor_names <- factor_names[all_factors]


source("sources/aggregate_v4_NEO_temperature.R")

# create results per temperature setting
temperatures = data_aggregated$temperature %>% unique()

for (temp in temperatures)
{
  sink(paste("outputs/output_desc_",measurement_version, "_temp", temp, ".txt"))
  
  data_temp <- data_aggregated %>% filter(temperature == temp)
  
# analyze facets of all essays
plots <- list()
i <- 1
for (facets in facets_list){
  plots[[i]] <- data_temp %>%
    verteilung_sechsfach(facets) + plot_annotation(title=paste(facets))
  ggsave(paste("graphics/density_factor_", i, "_", measurement_version, "_temp", temp, "_facets.png"), plot = plots[[i]], dpi=300, width = 8, height = 6)
  i <- i + 1
}
combined_plot <- (plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]])
ggsave(paste("graphics/density_", measurement_version, "_temp", temp, "_facets.png"), plot = combined_plot, dpi=300, width = 8, height = 16)


# descriptive statistics of all facets
desc.stats <- data_temp %>% 
  select(all_of(all_facets)) %>% 
  describe()
desc_df <- desc.stats %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  select(Variable, n, mean, sd, median, min, max) %>%
  mutate(
    across(c(mean, sd, median, min, max), ~round(.x, 2))
    )
psych_table <- desc_df %>%
  flextable() %>%
  set_header_labels(
    Variable = "Variable",
    n = "N",
    mean = "M",
    sd = "SD", 
    median = "Median",
    min = "Min",
    max = "Max"
  ) %>%
  theme_vanilla() %>%
  autofit() %>%
  align(j = 2:7, align = "center", part = "all")
save_as_docx(psych_table, path = paste("tables/desc_", measurement_version, "_temp", temp, "_facets.docx"))

# analyze OCEAN factors of all essays
plots <- list()
i <- 1
for (factor in all_factors){
  plots[[i]] <- data_temp %>%
    verteilung_factornames(factor)
  i <- i + 1
}
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plot_layout(ncol = 3)
ggsave(paste("graphics/density_", measurement_version, "_temp", temp, "_factors.png"), plot = combined_plot, dpi=300, width = 8, height = 6)

# descriptive statistics of all facets
desc.stats <- data_temp %>% 
  select(all_of(all_factors)) %>% 
  describe()
desc_df <- desc.stats %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  select(Variable, n, mean, sd, median, min, max) %>%
  mutate(
    across(c(mean, sd, median, min, max), ~round(.x, 2))
  )
psych_table <- desc_df %>%
  flextable() %>%
  set_header_labels(
    Variable = "Variable",
    n = "N",
    mean = "M",
    sd = "SD", 
    median = "Median",
    min = "Min",
    max = "Max"
  ) %>%
  theme_vanilla() %>%
  autofit() %>%
  align(j = 2:7, align = "center", part = "all")
save_as_docx(psych_table, path = paste("tables/desc_", measurement_version, "_temp", temp, "_factors.docx"))  # !!! ks-werte ergänzen!

ks.tests <- list()
i <- 1
# Kolmogorov-Smirnov-Tests
for (factor in all_factors){
vec <- data_temp %>%
  select(!!sym(factor)) %>% 
  pull(!!sym(factor))
ks.tests[[i]] <- ks.test(vec, "pnorm", mean(vec), sd(vec))
i <- i+1
}
print(ks.tests)
sink()
}

sink(paste("outputs/output_ANOVA_temperatures_",measurement_version, ".txt"))
# ANOVA der temperaturen
print("ANOVA of aggregated data, factor = temperature")
model_oneway <- aov(o_llm ~ temp.factor, data = data_aggregated)
summary(model_oneway)
model_oneway <- aov(c_llm ~ temp.factor, data = data_aggregated)
summary(model_oneway)
model_oneway <- aov(e_llm ~ temp.factor, data = data_aggregated)
summary(model_oneway)
model_oneway <- aov(a_llm ~ temp.factor, data = data_aggregated)
summary(model_oneway)
model_oneway <- aov(n_llm ~ temp.factor, data = data_aggregated)
summary(model_oneway)

sink()
