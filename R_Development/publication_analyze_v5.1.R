# Model version for printing
modelVersion <- "v5.1"

source("connect_database.R")
source("functions.R")
source("Factor-Names-EN.R")
all_factors <- c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
all_factor_names <- factor_names[all_factors]

o_facets <- c("of3b", "of1", "of2", "of5")
c_facets <- c("cf2b", "cf3b", "cf3", "cf5")
e_facets <- c("ef2", "ef3b", "ef4", "ef5")
a_facets <- c("af1b", "af1", "af3", "af6")
n_facets <- c("nf1", "nf4", "nf6")

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_names <- list("Creative Imagination", "Fantasy", "Aesthetics", "Ideas",
                  "Productiveness", "Responsibility", "Dutifulness", "Self-Discipline",
                  "Gregariousness", "Energy Level", "Activity", "Excitement-seeking", 
                  "Compassion", "Trust", "Altruism", "Tender-mindedness", 
                  "Anxiety", "Self-consciousness", "Vulnerability")

all_factors <- c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
all_factor_names <- factor_names[all_factors]


data <- tbl(con, "openai_analyzation_v5") %>% 
  select(essay_id, all_of(all_facets), model) %>% 
  filter(essay_id <=250) %>% 
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  collect()


source("aggregate_v5.R")

data_facets <- data_aggregated %>% 
  select(all_of(all_facets))
data_factors <- data_aggregated %>% 
  select(all_of(all_factors))

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

