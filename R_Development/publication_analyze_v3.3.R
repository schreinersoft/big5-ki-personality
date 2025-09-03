# Model version for printing
modelVersion <- "v3.3"

source("connect_database.R")
source("functions.R")
source("NEO-PI-R-Names-EN.R")

# combine datasets               DANGER !!! v2 in publication is v3 XXX
data <- tbl(con, "openai_analyzation_v2") %>% select(-updated_at) %>%
  collect() %>% 
  drop_na("of1")

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)


# remove Items from analyzation
o_facets <- o_facets[o_facets != "of3"] 
o_facets <- o_facets[o_facets != "of4"]

e_facets <- e_facets[e_facets != "ef1"]  
e_facets <- e_facets[e_facets != "ef6"]  

a_facets <- a_facets[a_facets != "af2"] 
a_facets <- a_facets[a_facets != "af5"] 

n_facets <- n_facets[n_facets != "nf2"]  
n_facets <- n_facets[n_facets != "nf5"] 

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]

source("aggregate_v2_NEO.R")

sink(paste("outputs/omega_analyzation_", modelVersion, ".txt"))
source("omega.R")
sink()




sink(paste("outputs/output_analyzation_", modelVersion, ".txt"))

source("macros_v2.R")

sink()


# CFA
model <- '
  Ofactor =~ of1 + of2 + of5 + of6
  Cfactor =~ cf1 + cf2 + cf3 + cf4 + cf5 + cf6
  Efactor =~ ef2 + ef3 + ef4 + ef5
  Afactor =~ af1 + af3 + af4 + af6
  Nfactor =~ nf1 + nf3 + nf4 + nf6
'

sink(paste("outputs/output_cfa_", modelVersion, ".txt"))
fit <- cfa(model, data = data_facets, 
           estimator = "ML")

summary(fit, fit.measures = TRUE, standardized = TRUE)
parameterEstimates(fit)
standardizedSolution(fit)  # -1... +1
reliability(fit) 
inspect(fit, "cor.lv")
modificationIndices(fit, sort = TRUE)
residuals(fit, type = "standardized")

sink()
