library(psych)
library(flextable)

source("connect_database.R")
source("functions.R")
source("NEO-PI-R-Names-EN.R")
source("Factor-Names.R")

# get data
data <- tbl(con, "openai_analyzation_v3") %>% 
  select(-updated_at) %>% 
  collect()

# set up facets
o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_factors <- c("O", "C", "E", "A", "N")
all_factor_names <- factor_names[all_factors]

# create aggregated results as llm measurements per Essay
essays_aggregations <- data %>%
  group_by(essay_id) %>%
  summarise(
    O = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    C = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    E = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    A = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    N = mean(c_across(all_of(n_facets)), na.rm = TRUE),
    of1 = mean(of1, na.rm = TRUE),
    cf1 = mean(cf1, na.rm = TRUE),
    ef1 = mean(ef1, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    nf1 = mean(nf1, na.rm = TRUE),
    of2 = mean(of2, na.rm = TRUE),
    cf2 = mean(cf2, na.rm = TRUE),
    ef2 = mean(ef2, na.rm = TRUE),
    af2 = mean(af2, na.rm = TRUE),
    nf2 = mean(nf2, na.rm = TRUE),
    of3 = mean(of3, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    ef3 = mean(ef3, na.rm = TRUE),
    af3 = mean(af3, na.rm = TRUE),
    nf3 = mean(nf3, na.rm = TRUE),
    .groups = "drop")


# analze facets of all essays
plots <- list()
i <- 1
for (facets in facets_list){
  plots[[i]] <- essays_aggregations %>%
    verteilung_dreifach(facets) + plot_annotation(title=paste(facets))
  i <- i + 1
}
combined_plot <- (plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]])
combined_plot
ggsave("graphics/density_openai_v1_facets.png", plot = combined_plot, dpi=300, width = 8, height = 8)


# descriptive statistics of all facets
desc.stats <- essays_aggregations %>% 
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

# analyze OCEAN factors of all essays
plots <- list()
i <- 1
for (factor in all_factors){
  plots[[i]] <- essays_aggregations %>%
    verteilung_factornames(factor)
  i <- i + 1
}
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plot_layout(ncol = 3)
combined_plot
ggsave("graphics/density_openai_v1_factors.png", plot = combined_plot, dpi=300, width = 8, height = 6)

# descriptive statistics of all facets
desc.stats <- essays_aggregations %>% 
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
save_as_docx(psych_table, path = "tables/desc_openai_v1_factors.docx")  # !!! ks-werte ergänzen!

library(purrr)
ks.tests <- list()
i <- 1
# Kolmogorov-Smirnov-Tests
for (factor in all_factors){
vec <- essays_aggregations %>%
  select(!!sym(factor)) %>% 
  pull(!!sym(factor))
ks.tests[[i]] <- ks.test(vec, "pnorm", mean(vec), sd(vec))
i <- i+1
}
ks.tests


