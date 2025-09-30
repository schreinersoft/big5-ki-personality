library(car)
library(effectsize)
library(tidyverse)

source("sources/output_folders.R")

source("sources/connect_database.R")
source("sources/transformation_functions.R")
source("sources/combined_names_EN.R")

# read all data of all models
for (model in model_list) {
  models[[model]] <- left_join(essays, db_read_model(model), by = c("id" = "essay_id")) %>% 
    rename(essay_id = id) %>% 
    drop_na()
}



# https://www.statistikwunder.de/post/ergebnisse-einer-einfaktoriellen-anova-im-apa-stil-berichten

# Funktion zum Extrahieren der Statistiken
extract_stats_anova <- function(crit, pred, data) {

  formula <- reformulate(crit, pred)
  aov_result <- aov(formula, data = data)
  aov_summary <- summary(aov_result)
  result <- list()
  
  result$f <- aov_summary[[1]][1, "F value"]
  result$p <- aov_summary[[1]][1, "Pr(>F)"]
  result$df_groups <- aov_summary[[1]][1, "Df"]  
  result$df_measures <- aov_summary[[1]][2, "Df"]
  result$eta2 <- effectsize::eta_squared(aov_result)$Eta2
  result$d <- abs(effectsize::cohens_d(formula(aov_result), data = data)$Cohens_d)

  residuals <- residuals(aov_result)
  results$sw <- shapiro.test(residuals)$p.value
  result$lev <- as.numeric(leveneTest(formula(aov_result), data = data)$`Pr(>F)`[1])

  vec <- data %>%
    select(!!sym(pred)) %>% 
    pull(!!sym(pred))
  result$ks <- ks.test(vec, "pnorm", mean(vec), sd(vec))


  return(result)
}

# Funktion zum Extrahieren der Statistiken
extract_stats_kruskal <- function(object) {
  
  h_result <- object$kw

  return(list(
    p = h_result$p.value,
    chi2 = h_result$statistic,
    eta2 = object$eta2$rank_eta_squared
  ))
}



rFromWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2) 
  r <- z/ sqrt(N)
  return(r)
}
# Funktion zum Extrahieren der Statistiken
extract_stats_wilcoxon <- function(object) {
  
  u_result <- object$wc
  N <- length(object$data)
  
  return(list(
    W = u_result$statistic[["W"]],
    p = u_result$p.value,
    r = rFromWilcox(u_result, N)
  ))
}





anova_results <- list()
kruskal_wallis_results <- list()
wilcoxon_results <- list()

for (model in model_list)
{
  anova_results[[model]] <- list()
  kruskal_wallis_results[[model]] <- list()
  wilcoxon_results[[model]] <- list()
  
  all_factors <- models[[model]] %>% 
    select(
      starts_with("o_"),
      starts_with("c_"),
      starts_with("e_"),
      starts_with("a_"),
      starts_with("n_")
    ) %>% 
    names()

  data <- models[[model]] %>% 
    mutate(across(all_factors, as.numeric))

  
  outcomes <- c("bin_o", "bin_c", "bin_e", "bin_a", "bin_n")
  names(outcomes) <- c("O", "C", "E", "A", "N")
  
  for(i in 1:5) {
    formula <- reformulate(outcomes[i], all_factors[i])
    anova_results[[model]][[factor_names[i]]] <- list(
      aov = aov(formula, data = data),
      data = models[[model]]
    )
    kruskal_wallis_results[[model]][[factor_names[i]]] <- list(
      kw = kruskal.test(formula, data = data),
      eta2 = rank_eta_squared(formula, data = data),
      data = models[[model]]
    )
    wilcoxon_results[[model]][[factor_names[i]]] <- list(
      wc = wilcox.test(formula, data = data),
      data = models[[model]]
    )
  }
}

es <- extract_stats_anova(anova_results[["v1.0"]][["O"]])

kw <- extract_stats_kruskal(kruskal_wallis_results[["v1.0"]][["O"]])

wc <- extract_stats_wilcoxon(wilcoxon_results[["noise"]][["O"]])

wc$r


# ANOVA Statistiken für alle Modelle und Messgrößen extrahieren
score_results <- list()
anova_results_df <- data.frame()
for(model in model_list) {
  row_data <- c(Modell = model)

  scores <- create_scores_frame(models[[model]])
  score_results[[model]] <- mean(scores$SCORE)
  
  for(factor in factor_names) {
    stats <- extract_stats_anova(anova_results[[model]][[factor]])
    
    normrow <- paste("S", factor, sep="")
    
    # Statistiken formatieren
    row_data <- c(row_data,
                  format_psych(sprintf("%.2f", stats$F)),
                  format_p_psych(stats$p),
                  format_psych(sprintf("%.2f", stats$eta2)),
                  format_psych(sprintf("%.2f", stats$d)),
                  format_p_psych(sprintf("%.3f", stats$sw)),
                  format_p_psych(sprintf("%.3f", stats$lev)),
                  format_psych(sprintf("%.1f", mean(scores[[normrow]])))
                  )
  }
  anova_results_df <- rbind(anova_results_df, row_data, stringsAsFactors = FALSE)
}

# Kruskal-Wallis-H-Test Statistiken für alle Modelle und Messgrößen extrahieren
kruskal_wallis_results_df <- data.frame()
for(model in model_list) {
  row_data <- c(Modell = model)
  
  scores <- create_scores_frame(models[[model]])
  
  for(factor in factor_names) {
    stats <- extract_stats_kruskal(kruskal_wallis_results[[model]][[factor]])
    
    normrow <- paste("S", factor, sep="")
    
    # Statistiken formatieren
    row_data <- c(row_data,
                  format_psych(sprintf("%.2f", stats$chi2)),
                  format_p_psych(stats$p),
                  format_psych(sprintf("%.3f", stats$eta2)),
                  format_psych(sprintf("%.1f", mean(scores[[normrow]])))
                  )
  }
  kruskal_wallis_results_df <- rbind(kruskal_wallis_results_df, row_data, stringsAsFactors = FALSE)
}


# Mann-Whitney/Wolcoxon-U-Test Statistiken für alle Modelle und Messgrößen extrahieren
wilcoxon_results_df <- data.frame()
for(model in model_list) {
  row_data <- c(Modell = model)
  
  scores <- create_scores_frame(models[[model]])
  
  for(factor in factor_names) {
    stats <- extract_stats_wilcoxon(wilcoxon_results[[model]][[factor]])
    
    normrow <- paste("S", factor, sep="")
  
    
    # Statistiken formatieren
    row_data <- c(row_data,
                  format_psych(sprintf("%.0f", stats$W)),
                  format_p_psych(stats$p),
                  format_psych(sprintf("%.3f", stats$r)),
                  format_psych(sprintf("%.1f", mean(scores[[normrow]])))
    )
  }
  wilcoxon_results_df <- rbind(wilcoxon_results_df, row_data, stringsAsFactors = FALSE)
}


anova_results_df$SCORE <- unlist(map(score_results, ~ sprintf("%.1f", .x)))
kruskal_wallis_results_df$SCORE <- unlist(map(score_results, ~ sprintf("%.1f", .x)))
wilcoxon_results_df$SCORE <- unlist(map(score_results, ~ sprintf("%.1f", .x)))


# ANOVA Auswertungen als Tabelle
# Spaltennamen setzen
colnames(anova_results_df) <- c("Modell", paste0(rep(factor_names, each = 7), "_", rep(c("F", "p", "e", "d", "sw", "lev", "S"), 5)), "SCORE")
# flextable erstellen
ft <- flextable(anova_results_df) %>%
  # Unterheader für Statistiken (wird zur zweiten Zeile)
  add_header_row(values = c("Modell", rep(c("F(2,248)", "p", "η²", "d", "sw", "lev", "SC"), 5), "SCORE"), colwidths = rep(1, 37)) %>%
  # Hauptheader hinzufügen (wird zur ersten Zeile)
  add_header_row(values = c("", factor_names, "Ø"), colwidths = c(1, rep(7, 5), 1)) %>%
  # Erste Spalte in der ersten Zeile mit "Modell" füllen
  #merge_v(j = 1, part = "header") %>%
  # Layout anpassen
  theme_box() %>%
  # Spaltenbreiten anpassen
  width(j = 1, width = 1.2) %>%
  width(j = 2:37, width = 0.6) %>%
  # Zentrierung
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  # Schriftgröße anpassen
  fontsize(size = 9, part = "all") %>%
  fontsize(j = 1, size = 10, part = "body") %>%
  # Kopfzeilen hervorheben
  bold(part = "header") %>%
  # Hintergrundfarben für bessere Lesbarkeit
  bg(i = 1, part = "header", bg = "#4472C4") %>%
  bg(i = 2, part = "header", bg = "#8DB4E2") %>%
  color(part = "header", color = "white")

# Tabelle anzeigen
print(ft)
save_as_docx(ft, path=paste(tables_output_folder, "/anovas.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/anovas.xlsx",sep=""))



# Kruskal-Wallis-H-Tests Auswertungen als Tabelle
# Spaltennamen setzen
colnames(kruskal_wallis_results_df) <- c("Modell", paste0(rep(factor_names, each = 4), "_", rep(c("χ²", "p", "η²", "SC"), 5)), "SCORE")
# flextable erstellen
ft <- flextable(kruskal_wallis_results_df) %>%
  add_header_row(values = c("Modell", rep(c("χ²", "p", "η²", "SC"), 5), "SCORE"), colwidths = rep(1, 22)) %>%
  add_header_row(values = c("", factor_names, "Ø"), colwidths = c(1, rep(4, 5), 1), 1) %>%
   theme_box() %>%
  width(j = 1, width = 1.2) %>%
  width(j = 2:22, width = 0.6) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  fontsize(j = 1, size = 10, part = "body") %>%
  bold(part = "header") %>%
  # Hintergrundfarben für bessere Lesbarkeit
  bg(i = 1, part = "header", bg = "#4472C4") %>%
  bg(i = 2, part = "header", bg = "#8DB4E2") %>%
  color(part = "header", color = "white")

# Tabelle anzeigen
print(ft)
save_as_docx(ft, path=paste(tables_output_folder, "/h-tests.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/h-tests.xlsx",sep=""))



# U-Tests Auswertungen als Tabelle
# Spaltennamen setzen
colnames(wilcoxon_results_df) <- c("Modell", paste0(rep(factor_names, each = 4), "_", rep(c("W", "p", "r", "SC"), 5)), "SCORE")
# flextable erstellen
ft <- flextable(kruskal_wallis_results_df) %>%
  # Unterheader für Statistiken (wird zur zweiten Zeile)
  add_header_row(values = c("Modell", rep(c("W", "p", "r", "SC"), 5), "SCORE"), colwidths = rep(1, 22)) %>%
  # Hauptheader hinzufügen (wird zur ersten Zeile)
  add_header_row(values = c("", factor_names, "Ø"), colwidths = c(1, rep(4, 5), 1), 1) %>%
  # Erste Spalte in der ersten Zeile mit "Modell" füllen
  #merge_v(j = 1, part = "header") %>%
  # Layout anpassen
  theme_box() %>%
  # Spaltenbreiten anpassen
  width(j = 1, width = 1.2) %>%
  width(j = 2:22, width = 0.6) %>%
  # Zentrierung
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  # Schriftgröße anpassen
  fontsize(size = 9, part = "all") %>%
  fontsize(j = 1, size = 10, part = "body") %>%
  # Kopfzeilen hervorheben
  bold(part = "header") %>%
  # Hintergrundfarben für bessere Lesbarkeit
  bg(i = 1, part = "header", bg = "#4472C4") %>%
  bg(i = 2, part = "header", bg = "#8DB4E2") %>%
  color(part = "header", color = "white")

# Tabelle anzeigen
print(ft)
save_as_docx(ft, path=paste(tables_output_folder, "/u-tests.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/h-tests.xlsx",sep=""))


