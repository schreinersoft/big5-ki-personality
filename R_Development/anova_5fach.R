library(flextable)
library(effectsize)
library(tidyverse)

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
model_list <- c("liwc", "v1.0","v1.1","v1.2","v2.0","v2.1","v2.2","v2.3","v3.0","v4.000", "v4.002", "v4.004", "v4.006", "v4.008", "v4.010", "v4.1")
#model_list <- c("liwc", "v1.0","v1.1")
factor_names <- c("O", "C", "E", "A", "N")

# read all data of all models
for (model in model_list)
{
  models[[model]] <- left_join(essays, db_read_model(model), by = c("id" = "essay_id")) %>% 
    rename(essay_id = id) %>% 
    drop_na()
}



# Funktion zum Extrahieren der Statistiken
extract_stats_anova <- function(aov_result, data) {
  # ANOVA Zusammenfassung
  aov_summary <- summary(aov_result)
  
  # F-Wert und p-Wert extrahieren
  f_value <- aov_summary[[1]][1, "F value"]
  p_value <- aov_summary[[1]][1, "Pr(>F)"]
  
  # Cohen's d und Hedge's g berechnen
  # Für One-Way ANOVA verwenden wir eta_squared als Basis für d
  effect_size <- effectsize::eta_squared(aov_result)
  eta_sq <- effect_size$Eta2[1]
  
  # Umrechnung von eta² zu Cohen's d (approximativ)
  cohens_d <- 2 * sqrt(eta_sq / (1 - eta_sq))
  
  # Hedge's g (Korrektur für kleine Stichproben)
  n <- nrow(data)
  hedges_g <- cohens_d * (1 - (3 / (4 * (n - 2) - 1)))
  
  scores <- create_scores_frame(data)
  
  return(list(
    p = p_value,
    F = f_value,
    d = cohens_d,
    g = hedges_g,
    sc = mean(scores$SCORE)
  ))
}

# Funktion zum Extrahieren der Statistiken
extract_stats_kruskal <- function(aov_result, data) {
  # F-Wert und p-Wert extrahieren
  chi2_value <- aov_result$statistic
  p_value <- aov_result$p.value

  n <- nrow(data)
  k <- kw$parameter + 1 # k = df + 1
  
  # Umrechnung von eta² zu Cohen's d (approximativ)
  eta2_value <- (chi2_value - k + 1) / (n - k)

  scores <- create_scores_frame(data)
  
  return(list(
    p = p_value,
    chi2 = chi2_value,
    eta2 = eta2_value,
    sc = mean(scores$SCORE)
  ))
}


format_p_psych <- function(p_value) {
  ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
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
  
  outcomes <- c("bin_o", "bin_c", "bin_e", "bin_a", "bin_n")
  names(outcomes) <- c("O", "C", "E", "A", "N")
  
  for(i in 1:5) {
    anova_results[[model]][[factor_names[i]]] <- list(
      aov = aov(reformulate(outcomes[i], all_factors[i]), data = models[[model]]),
      data = models[[model]]
    )
    kruskal_wallis_results[[model]][[factor_names[i]]] <- list(
      kw = kruskal.test(reformulate(outcomes[i], all_factors[i]), data = models[[model]]),
      data = models[[model]]
    )
  }
}

es <- extract_stats_anova(anova_results[["v1.0"]][["O"]]$aov, anova_results[["v1.0"]][["O"]]$data)

kw <- kruskal_wallis_results[["v1.0"]][["O"]]$kw


es$p < 0.001

# ANOVA Statistiken für alle Modelle und Messgrößen extrahieren
anova_results_df <- data.frame()
for(model in model_list) {
  row_data <- c(Modell = model)
  
  for(measure in factor_names) {
    stats <- extract_stats_anova(
      anova_results[[model]][[measure]]$aov,
      anova_results[[model]][[measure]]$data
    )
    
    # Statistiken formatieren
    row_data <- c(row_data,
                  format_p_psych(stats$p),
                  sprintf("%.2f", stats$F),
                  sprintf("%.2f", stats$d),
                  sprintf("%.2f", stats$g),
                  sprintf("%.2f", stats$sc)
                  )
  }
  anova_results_df <- rbind(anova_results_df, row_data, stringsAsFactors = FALSE)
}

# Kruskal-Wallis-H-Test Statistiken für alle Modelle und Messgrößen extrahieren
kruskal_wallis_results_df <- data.frame()
for(model in model_list) {
  row_data <- c(Modell = model)
  
  for(measure in factor_names) {
    stats <- extract_stats_kruskal(
      kruskal_wallis_results[[model]][[measure]]$kw,
      kruskal_wallis_results[[model]][[measure]]$data
    )
    
    # Statistiken formatieren
    row_data <- c(row_data,
                  format_p_psych(stats$p),
                  sprintf("%.2f", stats$chi2),
                  sprintf("%.3f", stats$eta2),
                  sprintf("%.2f", stats$sc)
                  )
  }
  kruskal_wallis_results_df <- rbind(kruskal_wallis_results_df, row_data, stringsAsFactors = FALSE)
}






# ANOVA Auswertungen als Tabelle
# Spaltennamen setzen
colnames(anova_results_df) <- c("Modell", paste0(rep(factor_names, each = 5), "_", rep(c("p", "F", "d", "g", "SC"), 5)))
# flextable erstellen
ft <- flextable(anova_results_df) %>%
  # Unterheader für Statistiken (wird zur zweiten Zeile)
  add_header_row(values = c("Modell", rep(c("p", "F", "d", "g", "SC"), 5)), colwidths = rep(1, 26)) %>%
  # Hauptheader hinzufügen (wird zur ersten Zeile)
  add_header_row(values = c("", factor_names), colwidths = c(1, rep(5, 5))) %>%
  # Erste Spalte in der ersten Zeile mit "Modell" füllen
  #merge_v(j = 1, part = "header") %>%
  # Layout anpassen
  theme_box() %>%
  # Spaltenbreiten anpassen
  width(j = 1, width = 1.2) %>%
  width(j = 2:26, width = 0.6) %>%
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




# Kruskal-Wallis-H-Tests Auswertungen als Tabelle
# Spaltennamen setzen
colnames(kruskal_wallis_results_df) <- c("Modell", paste0(rep(factor_names, each = 4), "_", rep(c("p", "χ²", "η²", "SC"), 5)))
# flextable erstellen
ft <- flextable(kruskal_wallis_results_df) %>%
  # Unterheader für Statistiken (wird zur zweiten Zeile)
  add_header_row(values = c("Modell", rep(c("p", "χ²", "η²", "SC"), 5)), colwidths = rep(1, 21)) %>%
  # Hauptheader hinzufügen (wird zur ersten Zeile)
  add_header_row(values = c("", factor_names), colwidths = c(1, rep(4, 5))) %>%
  # Erste Spalte in der ersten Zeile mit "Modell" füllen
  #merge_v(j = 1, part = "header") %>%
  # Layout anpassen
  theme_box() %>%
  # Spaltenbreiten anpassen
  width(j = 1, width = 1.2) %>%
  width(j = 2:21, width = 0.6) %>%
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

