# create results per temperature setting
temperatures = data_facets$temperature %>% unique()

all_scree_data <- data.frame()

for (temp in temperatures)
  {
  sink(paste("outputs/output_analyzation_", "_temp", temp, ".txt"))
  
  print(paste("Analyzation ",model_version))
  print("")
  print("Using facets:")
  print(all_facets)
  print("")
  
  data_temp <- data_facets %>% filter(temperature == temp) %>% select(all_of(all_facets))
  
  # Korrelationsmatrix
  cor_matrix <- cor(data_temp, use = "complete.obs")
  # Round to 2 decimal places
  cor_matrix_rounded <- round(cor_matrix, 2)
  print("Correlation Matrix:")
  print(cor_matrix_rounded)
  
  # Plot correlation Matrix
  png(paste("graphics/corrplot_", model_version, "_temp", temp, ".png"), width = 2400, height = 2400, res = 150)
  corrplot(cor_matrix_rounded, method = "color", type = "upper", 
           addCoef.col = "black", tl.cex = 0.8)
  dev.off()
  
  
  # Faktorenanalyse
  faModel <- fa(data_temp, nfactors = 5, rotate = "varimax", fm = "pa", residuals=TRUE)
  print(faModel)
  
  print("Residuals:")
  print(round(faModel$residual, 2))
  print("Eigenvalues:")
  print(round(faModel$values, 2))
  
  
  # Generiere Summary für Word
  fit_summary <- data.frame(
    RMSEA = faModel$RMSEA[1],           # RMSEA point estimate
    RMSEA_lower = faModel$RMSEA[2],     # RMSEA lower bound (90% CI)
    RMSEA_upper = faModel$RMSEA[3],     # RMSEA upper bound (90% CI)
    CFI = faModel$CFI,                  # Comparative Fit Index
    TLI = faModel$TLI,                  # Tucker-Lewis Index
    SRMR = faModel$rms,                 # Standardized Root Mean Square Residual
    X2 = faModel$STATISTIC,      # Chi-Square statistic
    p = faModel$PVAL              # p-value for Chi-Square
  )
  
  fit_data <- data.frame(
    Measure = c("RMSEA", "RMSEA 90% CI", "CFI", "TLI", "SRMR", "Chi-Square", "p-value"),
    Value = c(
      sprintf("%.3f", faModel$RMSEA[1]),
      sprintf("[%.3f, %.3f]", faModel$RMSEA[2], faModel$RMSEA[3]),
      sprintf("%.3f", faModel$CFI),
      sprintf("%.3f", faModel$TLI), 
      sprintf("%.3f", faModel$rms),
      sprintf("%.2f", faModel$STATISTIC),
      sprintf("%.3f", faModel$PVAL)
    ),
    Interpretation = c(
      ifelse(faModel$RMSEA[1] <= 0.05, "Sehr gut", 
             ifelse(faModel$RMSEA[1] <= 0.08, "Akzeptabel", "Schlecht")),
      "",
      ifelse(faModel$CFI >= 0.95, "Gut", "Ungeeignet"),
      ifelse(faModel$TLI >= 0.95, "Gut", "Ungeeignet"),
      ifelse(faModel$rms <= 0.08, "Gut", "Ungeeignet"), 
      ifelse(faModel$PVAL >= 0.05, "Gut (nicht signifikant)", "Schlecht (signifikant)"),
      ""
    )
  )
  
  ft <- flextable(fit_data) %>%
    set_header_labels(
      Measure = "Kenngröße",
      Value = "Value", 
      Interpretation = "Interpretation"
    ) %>%
    theme_vanilla() %>%
    align(align = "center", part = "header") %>%
    align(j = 2, align = "center") %>%
    width(j = 1, width = 1.5) %>%
    width(j = 2, width = 1) %>%
    width(j = 3, width = 1.8)
  save_as_docx(ft, path = paste("tables/measures_", model_version, "_temp", temp, ".docx"))
  
  
  # Generiere Faktorladungen Tabelle für Word
  loadings_matrix <- faModel$loadings[]
  
  # Convert to data frame and add item names
  loadings_df <- as.data.frame(loadings_matrix)
  loadings_df$h2 = faModel$communality
  loadings_df <- round(loadings_df, 2)  # Round to 3 decimal places
  
  # Add item names as first column (adjust item names as needed)
  loadings_df$Item <- rownames(loadings_matrix)
  loadings_df <- loadings_df[, c("Item", paste0("PA", 1:5), "h2")]  # Reorder columns
  # Row mit Eigenwerten einfügen
  eigenwerte <- round(faModel$values, 2)
  loadings_df <- loadings_df %>% 
    add_row(Item="Eigenwert",
            PA1 = eigenwerte[1],
            PA2 = eigenwerte[2],
            PA3 = eigenwerte[3],
            PA4 = eigenwerte[4],
            PA5 = eigenwerte[5],
            h2 = NULL,
            .before=1)
  
  ft <- flextable(loadings_df)
  ft <- ft %>%
    set_header_labels(
      Item = "",
      PA1 = "Faktor 1",
      PA2 = "Faktor 2", 
      PA3 = "Faktor 3",
      PA4 = "Faktor 4",
      PA5 = "Faktor 5",
      h2 = "h²"
    ) %>%
    theme_vanilla() %>%
    autofit() %>%
    align(align = "center", part = "header") %>%
    align(j = 2:6, align = "center", part = "body") %>%
    bold(part = "header", i = 1)
  save_as_docx(ft, path = paste("tables/loadings_", model_version, "_temp", temp, ".docx"))
  
  
  # Generiere Scree Plot 
  scree_data <- data.frame(
    Component = 1:length(faModel$values),
    Eigenvalue = faModel$values,
    Temperature = temp
  )
  screePlot <- scree_data %>% 
    ggplot(aes(x = Component, y = Eigenvalue)) +
    geom_point(size = 3) +
    geom_line() +
    labs(
      x = "Primärkomponente",
      y = "Eigenwert"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = 1:length(faModel$values))
  ggsave(paste("graphics/screeplot_" , model_version, "_temp", temp, ".png"), plot = screePlot, dpi=300, width = 8, height = 5)
  # collect all scree data for combined display at the end
  all_scree_data <- rbind(all_scree_data, scree_data)
  
  sink()
}

# combined scree plot
screePlot <- all_scree_data %>% 
  filter(Component <= 15) %>% 
  ggplot(aes(x = Component, y = Eigenvalue, fill = Temperature, group = Temperature)) +
  #scale_fill_brewer(type = "qual", palette = "Oranges", guide = "none") +
  geom_point(size = 3, shape = 21, aes(color = Temperature)) +
  geom_line(aes(color = Temperature), size = 0.8) +
  labs(
    x = "Primärkomponente",
    y = "Eigenwert",
    fill = "Temperatur",
    color = "Temperatur"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:length(faModel$values))
screePlot
ggsave(paste("graphics/screeplot_combined_" , model_version, ".png"), plot = screePlot, dpi=300, width = 8, height = 5)
