source("Factor-Names-EN.R")

all_factors <- c("O", "C", "E", "A", "N")
all_factor_names <- factor_names[all_factors]

# create aggregated results as llm measurements per Essay
llm_aggregated <- data_aggregated %>%
  rowwise() %>% 
  summarise(
    O = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    C = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    E = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    A = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    N = mean(c_across(all_of(n_facets)), na.rm = TRUE),
    essay_id = essay_idb,
    .groups = "drop")


# analze facets of all essays
plots <- list()
i <- 1
for (facets in facet_list){
  plots[[i]] <- llm_aggregated %>%
    verteilung_dreifach(facets) + plot_annotation(title=paste(facets))
  i <- i + 1
}
combined_plot <- (plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]])
combined_plot
ggsave(paste("graphics/density_", modelVersion, "_facets.png"), plot = combined_plot, dpi=300, width = 8, height = 8)


# descriptive statistics of all facets
desc.stats <- llm_aggregated %>% 
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
save_as_docx(psych_table, path = "tables/desc_openai_v1_facets.docx")

# descriptive statistics of all factors
desc.stats <- llm_aggregated %>% 
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
psych_table
save_as_docx(psych_table, path = paste("tables/desc_", modelVersion, "_factors.docx"))

sink( paste("outputs/desc_", modelVersion, ".txt"))
library(purrr)
ks.tests <- list()
i <- 1
# Kolmogorov-Smirnov-Tests
for (factor in all_factors){
vec <- llm_aggregated %>%
  select(!!sym(factor)) %>% 
  pull(!!sym(factor))
ks.tests[[i]] <- ks.test(vec, "pnorm", mean(vec), sd(vec))
i <- i+1
}
ks.tests
sink()


