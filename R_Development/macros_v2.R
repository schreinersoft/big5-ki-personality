print(paste("Analyzation ",modelVersion))

# create aggregated results of llm calculations
data_aggregated <- data %>%
  group_by(essay_id) %>%
  summarise(of1 = mean(of1, na.rm = TRUE),
            cf1 = mean(cf1, na.rm = TRUE),
            ef1 = mean(ef1, na.rm = TRUE),
            af1 = mean(af1, na.rm = TRUE),
            nf1 = mean(nf1, na.rm = TRUE),
            of2 = mean(of2, na.rm = TRUE),
            cf2 = mean(cf2, na.rm = TRUE),
            ef2 = mean(ef2, na.rm = TRUE),
            af2 = mean(af2, na.rm = TRUE),
            nf2 = mean(nf2, na.rm = TRUE),
            of3 = mean(of3, na.rm = TRUE),
            cf3 = mean(cf3, na.rm = TRUE),
            ef3 = mean(ef3, na.rm = TRUE),
            af3 = mean(af3, na.rm = TRUE),
            nf3 = mean(nf3, na.rm = TRUE),
            of4 = mean(of4, na.rm = TRUE),
            cf4 = mean(cf4, na.rm = TRUE),
            ef4 = mean(ef4, na.rm = TRUE),
            af4 = mean(af4, na.rm = TRUE),
            nf4 = mean(nf4, na.rm = TRUE),
            of5 = mean(of5, na.rm = TRUE),
            cf5 = mean(cf5, na.rm = TRUE),
            ef5 = mean(ef5, na.rm = TRUE),
            af5 = mean(af5, na.rm = TRUE),
            nf5 = mean(nf5, na.rm = TRUE),
            of6 = mean(of6, na.rm = TRUE),
            cf6 = mean(cf6, na.rm = TRUE),
            ef6 = mean(ef6, na.rm = TRUE),
            af6 = mean(af6, na.rm = TRUE),
            nf6 = mean(nf6, na.rm = TRUE),
            o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
            c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
            e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
            a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
            n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
            .groups = "drop") 

data_facets <- data_aggregated %>% 
  select(all_of(all_facets)) %>% 
  as_tibble()

# Korrelationsmatrix
cor_matrix <- cor(data_facets, use = "complete.obs")
# Round to 2 decimal places
cor_matrix_rounded <- round(cor_matrix, 2)
print("Correlation Matrix:")
print(cor_matrix_rounded)

# Plot correlation Matrix
png(paste("graphics/corrplot_", modelVersion, ".png"), width = 2400, height = 2400, res = 150)
corrplot(cor_matrix_rounded, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8)
dev.off()


# Faktorenanalyse
faModel <- fa(data_facets, nfactors = 5, rotate = "varimax", fm = "pa", residuals=TRUE)
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
save_as_docx(ft, path = paste("tables/measures_", modelVersion, ".docx"))


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
save_as_docx(ft, path = paste("tables/loadings_", modelVersion, ".docx"))


# Generiere Scree Plot 
scree_data <- data.frame(
  Component = 1:length(faModel$values),
  Eigenvalue = faModel$values
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
ggsave(paste("graphics/screeplot_" , modelVersion, ".png"), plot = screePlot, dpi=300, width = 8, height = 5)

