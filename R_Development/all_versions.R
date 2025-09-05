### Collector for all models, data and analyzations

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("transformation_functions.R")
source("connect_database.R")


modelVersion <- "v5.1"

data <- tbl(con, "openai_analyzation_v5") %>% 
  select(essay_id, all_of(all_facets), model) %>% 
  filter(essay_id <=250) %>% 
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  collect()

data_aggregated <- model_aggregate(data)

source("ocean_plot.R")

sink(paste("outputs/omega_analyzation_", modelVersion, ".txt"))
source("omega.R")
sink()

sink(paste("outputs/output_analyzation_", modelVersion, ".txt"))

source("macros_v5.R")

sink()


essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
data_joined <- left_join(essays, data_aggregated, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)





# ANOVA der Binärgruppen
model_oneway <- aov(o_llm ~ o_binary, data = data_joined)
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = data_joined)
summary(model_oneway)
model_oneway <- aov(e_llm ~ e_binary, data = data_joined)
summary(model_oneway)
model_oneway <- aov(a_llm ~ a_binary, data = data_joined)
summary(model_oneway)
model_oneway <- aov(n_llm ~ n_binary, data = data_joined)
summary(model_oneway)


binary <- "0"
# plot all facet groups
plots <- list()
i <- 1
for (facet in all_facets){
  plots[[i]] <- data_aggregated %>%
    filter(o_binary=binary) %>% 
    verteilung(facet)
  i <- i + 1
}
combined_plot <- (plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]])
ggsave(paste("graphics/density_", modelVersion, "_facets_", binary,"_O.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <-  (plots[[5]] | plots[[6]] | plots[[7]] | plots[[8]])
ggsave(paste("graphics/density_", modelVersion, "_facets", binary,"_C.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <-  (plots[[9]] | plots[[10]] | plots[[11]] | plots[[12]])
ggsave(paste("graphics/density_", modelVersion, "_facets", binary,"_E.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <- (plots[[13]] | plots[[14]] | plots[[15]] | plots[[16]])
ggsave(paste("graphics/density_", modelVersion, "_facets", binary,"_A.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

combined_plot <- (plots[[17]] | plots[[18]] | plots[[19]])
ggsave(paste("graphics/density_", modelVersion, "_facets", binary,"_N.png"), plot = combined_plot, dpi=300, width = 8, height = 4)

