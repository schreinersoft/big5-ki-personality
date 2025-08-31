# Model version for printing
modelVersion <- "v2.0"

source("connect_database.R")
source("functions.R")
source("NEO-PI-R-Names-EN.R")

# combine datasets               DANGER !!! v2 in publication is v3 XXX
data <- tbl(con, "openai_analyzation_v3") %>% select(-updated_at) %>%
  collect() %>% 
  filter(essay_id <= 60) %>% 
  drop_na("of1")

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]

sink(paste("outputs/output_analyzation_", modelVersion, ".txt"))

print("Using facets:")
print(all_facets)

source("macros_v2.R")

sink()
