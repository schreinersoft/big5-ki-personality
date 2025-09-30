# Model version for printing
model_version <- "v1.1"

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/BFI-2-Names-EN.R")

# combine datasets
essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
openai <- tbl(con, "openai_analyzation") %>% 
  select(-updated_at) %>% 
  collect()
# join only rows with values
openai_joined <- left_join(essays, openai, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)



o_facets <- paste0("of", 1:3)
c_facets <- paste0("cf", 1:3)
e_facets <- paste0("ef", 1:3)
a_facets <- paste0("af", 1:3)
n_facets <- paste0("nf", 1:3)

e_facets <- e_facets[e_facets != "ef2"]  # schritt 2
#e_facets <- e_facets[e_facets != "ef1"]  # schritt 3

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)

facet_table <- openai_joined %>% 
  select(all_of(all_facets)) %>% 
  as_tibble()

source("sources/aggregate_v1_BFI.R")

sink(paste("outputs/omega_analyzation_", model_version, ".txt"))
source("sources/omega.R")
sink()


sink(paste("outputs/output_analyzation_", model_version, ".txt"))

source("sources/macros_v1.R")

sink()