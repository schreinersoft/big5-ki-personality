fa_original <- fa(data_facets, nfactors = 5, scores = "regression")

scoring_parameter <- list(
  # Scoring-Gewichte
  weights = fa_original$weights,
  
  # Mittelwerte der ursprünglichen Daten
  means = colMeans(data_facets, na.rm = TRUE),
  
  # Standardabweichungen der ursprünglichen Daten  
  sds = apply(data_facets, 2, sd, na.rm = TRUE),
  
  # Faktorladungen (zur Validierung)
  loadings = fa_original$loadings,
  
  # Variablennamen
  variable_names = colnames(data_facets),
  
  # Original Faktor-Scores (für Vergleiche)
  original_scores = fa_original$scores
)
