# Model version for printing
measurement_version <- "v2.0"

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/NEO-PI-R-Names-EN.R")

# combine datasets               DANGER !!! v2 in publication is v3 XXX
data <- tbl(con, "openai_analyzation_v3") %>% select(-updated_at) %>%
  collect() %>% 
  drop_na("of1")

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]


source("sources/aggregate_v2_NEO.R")

sink(paste("outputs/omega_analyzation_", measurement_version, ".txt"))
source("sources/omega.R")
sink()

sink(paste("outputs/output_analyzation_", measurement_version, ".txt"))

source("sources/macros_v2.R")

sink()
