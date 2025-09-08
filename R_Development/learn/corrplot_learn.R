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
  
