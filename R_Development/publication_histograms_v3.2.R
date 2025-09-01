modelVersion <- "v3.2"

source("connect_database.R")
source("functions.R")
source("BFI-2-Names-EN.R")

# Hole Messungen
data <- tbl(con, "openai_analyzation_v2") %>% 
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

# Histogram of Facets on one Essay
essay_number <- 42

plots <- list()
i <- 1
for (facets in facets_list){
  plots[[i]] <- data %>%
    filter(essay_id == essay_number) %>% 
    histogramm_sechsfach(facets) + plot_annotation(title=paste(facets))
  ggsave(paste("graphics/histograms_", i, "_", modelVersion, "essay_", essay_number, ".png"), plot = plots[[i]], dpi=300, width = 8, height = 6)
  i <- i + 1
}
combined_plot <- (plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]])
combined_plot
ggsave(paste("graphics/histograms_", modelVersion, "essay_", essay_number, ".png"), plot = combined_plot, dpi=300, width = 8, height = 8)

# descriptive statistics on one Essay
desc.stats <- data %>% 
  filter(essay_id==essay_number) %>% 
  select(all_of(all_facets)) %>% 
  describe()
# In Dataframe umwandeln und formatieren
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
    Variable = "Facette",
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
save_as_docx(psych_table, path = "tables/desc_openai_v1_essay_42.docx")

