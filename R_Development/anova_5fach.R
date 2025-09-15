library(flextable)
library(effectsize)
library(tidyverse)
library(writexl)

source("connect_database.R")

essays <- tbl(con, "essays") %>% 
  filter(id <=250) %>% 
  collect() %>% 
  mutate(
    bin_o = ifelse(o_binary == "1", 1, 0),
    bin_c = ifelse(c_binary == "1", 1, 0),
    bin_e = ifelse(e_binary == "1", 1, 0),
    bin_a = ifelse(a_binary == "1", 1, 0),
    bin_n = ifelse(n_binary == "1", 1, 0)) %>% 
  select(-text, -author, -all_of(ends_with("binary")))

models <- list()
model_list <- c("liwc", "minej",
                "v1.0","v1.1","v1.2",
                "v2.0","v2.1","v2.2","v2.3",
                "v3.0",
                "v4.000", "v4.002", "v4.004", "v4.006", "v4.008", "v4.010", "v4.1",
                "v5.X", "v5.0", "v5.0n", "v5.1", "v5.1n")
#model_list <- c("liwc", "v1.0","v1.1")
factor_names <- c("O", "C", "E", "A", "N")

# read all data of all models
for (model in model_list) {
  models[[model]] <- left_join(essays, db_read_model(model), by = c("id" = "essay_id")) %>% 
    rename(essay_id = id) %>% 
    drop_na()
}



# https://www.statistikwunder.de/post/ergebnisse-einer-einfaktoriellen-anova-im-apa-stil-berichten

# Funktion zum Extrahieren der Statistiken
extract_stats_anova <- function(aov_result) {
  # ANOVA Zusammenfassung
  aov_summary <- summary(aov_result$aov)
  
  # F-Wert und p-Wert extrahieren
  f_value <- aov_summary[[1]][1, "F value"]
  p_value <- aov_summary[[1]][1, "Pr(>F)"]
  df_groups <- aov_summary[[1]][1, "Df"]  
  df_measures <- aov_summary[[1]][2, "Df"]
  
  # Cohen's d und Hedge's g berechnen
  effect_size <- effectsize::eta_squared(aov_result$aov)

  scores <- create_scores_frame(aov_result$data)
  
  return(list(
    p = p_value,
    F = f_value,
    dfg = df_groups,
    dfm = df_measures,
    eta2 = effect_size$Eta2,
    d = abs(aov_result$cohens_d$Cohens_d),
    sc = mean(scores$SCORE)
  ))
}

# Funktion zum Extrahieren der Statistiken
extract_stats_kruskal <- function(h_result, data) {

  return(list(
    p = h_result$p.value,
    chi2 = h_result$statistic,
    eta2 = effectsize::eta_squared(h_result$model, ci = 0.95)
  ))
}

anova_results <- list()
kruskal_wallis_results <- list()

for (model in model_list)
{
  anova_results[[model]] <- list()
  kruskal_wallis_results[[model]] <- list()
  
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
    mutate(across(starts_with("bin_"), as.factor),
           across(all_factors, as.numeric)
    )
  
  outcomes <- c("bin_o", "bin_c", "bin_e", "bin_a", "bin_n")
  names(outcomes) <- c("O", "C", "E", "A", "N")
  
  for(i in 1:5) {
    formula <- reformulate(outcomes[i], all_factors[i])
    anova_results[[model]][[factor_names[i]]] <- list(
      aov = aov(formula, data = data),
      cohens_d = effectsize::cohens_d(formula, data = data),
      data = models[[model]]
    )
    kruskal_wallis_results[[model]][[factor_names[i]]] <- list(
      kw = kruskal.test(formula, data = data),
      data = models[[model]]
    )
  }
}

es <- extract_stats_anova(anova_results[["v1.0"]][["O"]])

kw <- kruskal_wallis_results[["v1.0"]][["O"]]$kw


es$p < 0.001

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
    stats <- extract_stats_kruskal(
      kruskal_wallis_results[[model]][[factor]]$kw,
      kruskal_wallis_results[[model]][[factor]]$data
    )
    
    normrow <- paste("S", factor, sep="")
    
    # Statistiken formatieren
    row_data <- c(row_data,
                  format_p_psych(stats$p),
                  format_psych(sprintf("%.2f", stats$chi2)),
                  format_psych(sprintf("%.3f", stats$eta2)),
                  format_psych(sprintf("%.1f", mean(scores[[normrow]])))
                  )
  }
  kruskal_wallis_results_df <- rbind(kruskal_wallis_results_df, row_data, stringsAsFactors = FALSE)
}

anova_results_df$SCORE <- unlist(map(score_results, ~ sprintf("%.1f", .x)))
kruskal_wallis_results_df$SCORE <- unlist(map(score_results, ~ sprintf("%.1f", .x)))


# ANOVA Auswertungen als Tabelle
# Spaltennamen setzen
colnames(anova_results_df) <- c("Modell", paste0(rep(factor_names, each = 5), "_", rep(c("p", "F", "e", "d", "S"), 5)), "SCORE")
# flextable erstellen
ft <- flextable(anova_results_df) %>%
  # Unterheader für Statistiken (wird zur zweiten Zeile)
  add_header_row(values = c("Modell", rep(c("p", "F(2,248)", "η²", "d", "SC"), 5), "SCORE"), colwidths = rep(1, 27)) %>%
  # Hauptheader hinzufügen (wird zur ersten Zeile)
  add_header_row(values = c("", factor_names, "Ø"), colwidths = c(1, rep(5, 5), 1)) %>%
  # Erste Spalte in der ersten Zeile mit "Modell" füllen
  #merge_v(j = 1, part = "header") %>%
  # Layout anpassen
  theme_box() %>%
  # Spaltenbreiten anpassen
  width(j = 1, width = 1.2) %>%
  width(j = 2:27, width = 0.6) %>%
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
colnames(kruskal_wallis_results_df) <- c("Modell", paste0(rep(factor_names, each = 4), "_", rep(c("p", "χ²", "η²", "SC"), 5)), "SCORE")
# flextable erstellen
ft <- flextable(kruskal_wallis_results_df) %>%
  # Unterheader für Statistiken (wird zur zweiten Zeile)
  add_header_row(values = c("Modell", rep(c("p", "χ²", "η²", "SC"), 5), "SCORE"), colwidths = rep(1, 22)) %>%
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
save_as_docx(ft, path=paste(tables_output_folder, "/h-tests.docx",sep=""))
write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/h-tests.xlsx",sep=""))


