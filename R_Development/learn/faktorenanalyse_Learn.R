install.packages("lavaan")  # Install if not already installed
library(lavaan)             # Load the package

facets <- read_csv("learn/OCfacets-fake_comma.csv")

model <- '
  Ofactor =~ O1 + O2 + O3
  Cfactor =~ C1 + C2 + C3
'

fit <- cfa(model, data = facets)

summary(fit, fit.measures = TRUE, standardized = TRUE)

