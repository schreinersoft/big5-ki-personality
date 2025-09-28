library(corrr)
library(tidyverse)
library(Hmisc)
library(flextable)

source("transformation_functions.R")

result <- rcorr(as.matrix(mtcars))
print(result)


result$r


##################
data_matrix <- as.matrix(mtcars[, c("mpg", "hp", "wt", "qsec", "disp")])

create_corr_matrix <- function(matrix) {

# Korrelationen und p-Werte berechnen
rcorr_result <- rcorr(data_matrix, type = "pearson")

# Matrizen extrahieren
cor_matrix <- rcorr_result$r  # Korrelationen
p_matrix <- rcorr_result$P    # p-Werte

cor_matrix <- apply(cor_matrix, c(1,2), format_psych)
p_matrix <- apply(p_matrix, c(1,2), format_p_psych)

# Kombinierte Matrix erstellen
combined_matrix <- cor_matrix

# Unteres Dreieck mit p-Werten füllen
combined_matrix[lower.tri(combined_matrix)] <- p_matrix[lower.tri(p_matrix)]

# Diagonale leeren (optional)
diag(combined_matrix) <- NA

return (combined_matrix %>% 
  as.data.frame() %>% 
  flextable())

}
cor_matrix <- mtcars %>% 
  select(mpg, hp, wt, qsec) %>%
  correlate(method = "pearson")


  data_facets <- data_aggregated %>%
    select(starts_with(("of")),
           starts_with(("cf")),
           starts_with(("nf")))
  
  cor_matrix <- corr.test(data_facets, use = "complete.obs", method="pearson")
  cor_matrix_rounded <- round(cor_matrix$r, 2)
  print("Correlation Matrix (Pearson's r):")
  print(cor_matrix_rounded)
  print("p-Values:")
  print(round(cor_matrix$p, 3))
  
  
  size <- dim(data_facets)[[2]] * 90

  png("C:/temp/facets.png", width = size, height = size, res = 150)
  corrplot(cor_matrix_rounded, method = "color", type = "upper", 
           addCoef.col = "black", tl.cex = 0.8)#
  dev.off()
  

  