model_version <- "v5.1"

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/BFI-2-Names-EN.R")


###############################
# Histogram of Facets on one Essay
essay_number <- 42

plots <- list()
i <- 1
for (facets in facet_list){
  for (facet in facets){
    print(paste("Facet: ", i))
  plots[[i]] <- data_aggregated %>%
    filter(essay_idb == essay_number) %>% 
    histogramm(facet) + plot_annotation(title=paste(facet_names[[i]]))
  ggsave(paste("graphics/histograms_", i, "_", model_version, "essay_", essay_number, ".png"), plot = plots[[i]], dpi=300, width = 8, height = 6)
  i <- i + 1
  }
}
combined_plot <- (plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]])
combined_plot
ggsave(paste("graphics/histograms_", model_version, "essay_", essay_number, ".png"), plot = combined_plot, dpi=300, width = 8, height = 8)

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

