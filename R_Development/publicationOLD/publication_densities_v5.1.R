library(purrr)

source("sources/Factor-Names-EN.R")

all_factors <- c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
all_factor_names <- factor_names[all_factors]


# plot all facet groups
plots <- list()
i <- 1
for (facet in all_facets){
  plots[[i]] <- data_aggregated %>%
    verteilung(facet)
  i <- i + 1
}
combined_plot <- (plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]])
ggsave(paste("graphics/density_", measurement_version, "_facets_O.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <-  (plots[[5]] | plots[[6]] | plots[[7]] | plots[[8]])
ggsave(paste("graphics/density_", measurement_version, "_facets_C.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <-  (plots[[9]] | plots[[10]] | plots[[11]] | plots[[12]])
ggsave(paste("graphics/density_", measurement_version, "_facets_E.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <- (plots[[13]] | plots[[14]] | plots[[15]] | plots[[16]])
ggsave(paste("graphics/density_", measurement_version, "_facets_A.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <- (plots[[17]] | plots[[18]] | plots[[19]])
ggsave(paste("graphics/density_", measurement_version, "_facets_N.png"), plot = combined_plot, dpi=300, width = 8, height = 4)


# descriptive statistics of all facets
desc.stats <- data_aggregated %>% 
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
psych_table
save_as_docx(psych_table, path = paste("tables/desc_", measurement_version, "_facets.docx"))

# descriptive statistics of all factors
desc.stats <- data_aggregated %>% 
  select(o_llm, c_llm, e_llm, a_llm, n_llm) %>% 
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
psych_table
save_as_docx(psych_table, path = paste("tables/desc_", measurement_version, "_factors.docx"))


# KS Values
sink( paste("outputs/desc_", measurement_version, ".txt"))
ks.tests <- list()
i <- 1
# Kolmogorov-Smirnov-Tests
for (factor in all_factors){
vec <- data_aggregated %>%
  select(!!sym(factor)) %>% 
  pull(!!sym(factor))
ks.tests[[i]] <- ks.test(vec, "pnorm", mean(vec), sd(vec))
i <- i+1
}
ks.tests
sink()

ks1 <- ks.tests[[1]]

ks1$statistic
