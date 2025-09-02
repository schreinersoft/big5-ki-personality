# Model version for printing
modelVersion <- "v1.3"

source("connect_database.R")
source("functions.R")
source("BFI-2-Names-EN.R")

# get data
data <- tbl(con, "openai_analyzation") %>% 
  select(-updated_at) %>% 
  collect()


o_facets <- paste0("of", 1:3)
c_facets <- paste0("cf", 1:3)
e_facets <- paste0("ef", 1:3)
a_facets <- paste0("af", 1:3)
n_facets <- paste0("nf", 1:3)

e_facets <- e_facets[e_facets != "ef1"]
#n_facets <- n_facets[n_facets != "nf3"]

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)

source("aggregate_v1_BFI.R")

sink(paste("outputs/omega_analyzation_", modelVersion, ".txt"))
source("omega.R")
sink()

sink(paste("outputs/output_analyzation_", modelVersion, ".txt"))

source("macros_v1.R")

sink()
