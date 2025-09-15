# Alle Tests auf einmal
fa_assumptions_check <- function(data) {
  cor_mat <- cor(data, use = "complete.obs")
  
  # KMO
  kmo_result <- KMO(cor_mat)
  
  # Bartlett
  bartlett_result <- cortest.bartlett(cor_mat, n = nrow(data))
  
  # Stichprobengröße
  sample_ratio <- nrow(data) / ncol(data)
  
  # Determinante
  det_value <- det(cor_mat)
  
  cat("=== FAKTORENANALYSE VORAUSSETZUNGEN ===\n")
  cat("KMO overall:", round(kmo_result$MSA, 3), "\n")
  cat("Bartlett p-value:", bartlett_result$p.value, "\n")
  cat("Cases per variable:", round(sample_ratio, 1), "\n")
  cat("Determinant:", format(det_value, scientific = TRUE), "\n")
  cat("Min KMO individual:", round(min(kmo_result$MSAi), 3), "\n")
  
  return(list(kmo = kmo_result, bartlett = bartlett_result))
}

assumptions <- fa_assumptions_check(data_facets)
