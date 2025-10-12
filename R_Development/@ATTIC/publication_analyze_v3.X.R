# Model version for printing
measurement_version <- "v3.X"

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/NEO-PI-R-Names-EN.R")

#! TEST ! Analyzation of all, not aggregated measurements! -> no effect!


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
#o_facets <- o_facets[o_facets != "of1"] 
#o_facets <- o_facets[o_facets != "of2"]
#o_facets <- o_facets[o_facets != "of3"]

#c_facets <- c_facets[c_facets != "cf2"]
#c_facets <- c_facets[c_facets != "cf5"]
#c_facets <- c_facets[c_facets != "cf6"]

#e_facets <- e_facets[e_facets != "ef1"]  
#e_facets <- e_facets[e_facets != "ef3"]  
#e_facets <- e_facets[e_facets != "ef6"]  

#a_facets <- a_facets[a_facets != "af1"] 
#a_facets <- a_facets[a_facets != "af3"]
#a_facets <- a_facets[a_facets != "af6"] 

#n_facets <- n_facets[n_facets != "nf3"]  
#n_facets <- n_facets[n_facets != "nf5"] 
#n_facets <- n_facets[n_facets != "nf6"]  

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]


sink(paste("outputs/output_analyzation_", measurement_version, ".txt"))

source("sources/macros_v3.X_all_measurements.R")

sink()


# CFA
model <- '
  Ofactor =~ of1 + of2 + of3 + of4 + of5 + of6
  Cfactor =~ cf1 + cf2 + cf3 + cf4 + cf5 + cf6
  Efactor =~ ef1 + ef2 + ef3 + ef4 + ef5 + ef6
  Afactor =~ af1 + af2 + af3 + af4 + af5 + af6
  Nfactor =~ nf1 + nf2 + nf3 + nf4 + nf5 + nf6
'

sink(paste("outputs/output_cfa_", measurement_version, ".txt"))
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
