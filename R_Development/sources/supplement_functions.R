library(tidyverse)
library(psych)
library(flextable)
library(corrr)
library(Hmisc)

write_txt<- function(data, kind, instrument_version){
  sink(paste(supplement_output_folder, "/", kind, "_", instrument_version, ".txt", sep=""))
  summary(data)
  sink()
      
}


supp_analyse_essay_item <- function(data, model_instrument_version, essay_number=NULL) {

  if (is.null(essay_number)) {
    facets <- data %>% 
      select(starts_with(("of")),
            starts_with(("cf")),
            starts_with(("ef")),
            starts_with(("af")),
            starts_with(("nf")))

    essay_number <- "ALL"
    kind <- paste("item_statistics_essay_", essay_number, sep="")
  } else {
    facets <- data %>% 
      filter(essay_id == essay_number) %>% 
      select(starts_with(("of")),
             starts_with(("cf")),
             starts_with(("ef")),
             starts_with(("af")),
             starts_with(("nf")))
      kind <- "item_statistics"
  }

  stats <- facets %>% 
    psych::describe() %>% 
    select(-mad, -trimmed, -range, -skew, -kurtosis, -se) %>% 
    round(2) %>% 
    as.data.frame()
  

  ft <- stats %>% 
    rownames_to_column("variable") %>%
    mutate(
      name = variable_names[variable]) %>% 
    select(-vars) %>% 
    relocate(name, .before=n) %>% 
    flextable() %>% 
    set_header_labels(
      variable = "Facette",
      name = "Name",
      n = "N",
      mean = "M",
      sd = "SD", 
      median = "Median",
      min = "Min",
      max = "Max"
    ) %>%
    theme_alafoli() %>%
    italic(part = "header", j = c(3,4,5)) %>% 
    autofit()
  
  #save_as_docx(ft, path = paste(supplement_output_folder, kind, ".docx", sep=""))
  #write_xlsx(stats, path=paste(supplement_output_folder, kind, ".xlsx",sep=""))
  write_txt(stats, kind, model_instrument_version)
  
  return(ft)
}



########################
# descriptive statistics of all facets
supp_analyze_factors <- function(data, model_instrument_version, likert_max = 9)
{
  all_factors <- data %>% select(
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")) %>% names()

  desc_factors <- data %>% 
    select(all_of(all_factors)) %>% 
    psych::describe() %>%
    as.data.frame() %>%
    select(-mad, -trimmed)

  # analysiere jede faktorgruppe einzeln
  additions_factors <- data.frame()

  for (factor in all_factors) {
    factor_items <- data %>% 
      select(starts_with(paste0(substr(factor, 1, 1), "f"))) %>%
      names()
    
    omega_result <- psych::omega(data[factor_items], nfactors = 1, flip=FALSE, plot=FALSE)
    summary(omega_result)
    alpha_result <- psych::alpha(data[factor_items])
    summary(alpha_result)

    factor_stats <- data.frame(
      Variable = factor,
      alpha = omega_result$alpha,
      omega = omega_result$omega.tot
      )
    
    additions_factors <- rbind(additions_factors, factor_stats)
}

  summary_factors <- desc_factors %>%
    rownames_to_column("Variable") %>%
    rowwise() %>%
    mutate(Name = variable_names[Variable],
           ks = suppressWarnings(ks.test(data[[Variable]], "pnorm", 
                                         mean(data[[Variable]], na.rm = TRUE), 
                                         sd(data[[Variable]], na.rm = TRUE))$p.value),
           sw = shapiro.test(data[[Variable]])$p.value) %>% 
    select(Variable, Name, n, mean, sd, median, range, ks, sw) %>%
    mutate(
      across(c(mean, sd, median, range), ~format_psych(.x)),
      across(c(ks, sw), ~format_p_psych(.x))
    )
  
  summary_factors <- summary_factors %>%
    left_join(
      additions_factors %>% 
        mutate(
          alpha = format_psych(alpha),
          omega = format_psych(omega)
        ),
      by = "Variable"
    )
  
  # Flextable erstellen
  factor_table <- summary_factors %>%
    select(-Variable) %>% 
    flextable() %>%
    set_header_labels(
      #Variable = "Faktor",
      Name = "Name",
      n = "N",
      mean = "M",
      sd = "SD", 
      median = "Median",
      range = "Spannweite",
      ks = "K-S",
      sw = "S-W",
      alpha = "\u03B1",
      omega = "\u03C9"
    ) %>%
    theme_alafoli() %>%
    italic(part = "header", j = c(3,4)) %>% 
    align(j = 2:10, align = "center", part = "all") %>% 
    
    autofit()
  
  kind <- paste("/stats_facets_", model_instrument_version, sep="")
  
  write_txt(summary_factors, kind, model_instrument_version)
  return (factor_table)
}

#################################################################### Facets
supp_analyze_facets <- function(data, model_instrument_version, likert_max = 9)
{
  all_factors <- data %>% select(
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")) %>% names()
  o_facets <- data %>% select(starts_with(("of"))) %>% names()
  c_facets <- data %>% select(starts_with(("cf"))) %>% names()
  e_facets <- data %>% select(starts_with(("ef"))) %>% names()
  a_facets <- data %>% select(starts_with(("af"))) %>% names()
  n_facets <- data %>% select(starts_with(("nf"))) %>% names()
  factor_facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
  all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
  
  desc_facets <- data %>% 
    select(all_of(all_facets)) %>% 
    psych::describe() %>% 
    as.data.frame() %>% 
    select(-mad, -trimmed)

  additions_facets <- data.frame()
  
  for (factor in all_factors) {
    factor_items <- data %>% 
      select(starts_with(paste0(substr(factor, 1, 1), "f"))) %>%
      names()
    
    alpha_result <- psych::alpha(data[factor_items])
    summary(alpha_result)
    
    facet_stats <- data.frame(
      Variable = rownames(alpha_result$item.stats),
      r.cor = alpha_result$item.stats$r.cor
    )
    
    additions_facets <- rbind(additions_facets, facet_stats)
    
  }
  
  
  summary_facets <- desc_facets %>%
    rownames_to_column("Variable") %>%
    rowwise() %>%
    mutate(Name = variable_names[Variable],
           difficulty = round((((mean-1) / likert_max)*100)), 
           ks = suppressWarnings(ks.test(data[[Variable]], "pnorm", 
                                         mean(data[[Variable]], na.rm = TRUE), 
                                         sd(data[[Variable]], na.rm = TRUE))$p.value),
           sw = shapiro.test(data[[Variable]])$p.value) %>% 
    select(Variable, Name, n, mean, sd, median, min, max, ks, sw, difficulty) %>%
    mutate(
      across(c(mean, sd, median, min, max), ~format_psych(.x)),
      across(c(ks, sw), ~format_p_psych(.x))
    )
  
  summary_facets <- summary_facets %>%
    left_join(
      additions_facets %>% 
        mutate(
          r.cor = format_psych(r.cor)
        ),
      by = "Variable"
    )
  
  # Flextable erstellen
  facet_table <- summary_facets %>%
    select(-Name) %>% 
    flextable() %>%
    set_header_labels(
      Variable = "Facette",
      n = "N",
      mean = "M",
      sd = "SD", 
      median = "Median",
      min = "Min",
      max = "Max",
      r.cor = "TS",
      difficulty = "IS",
      ks = "K-S",
      sw = "S-W"
    ) %>%
    theme_alafoli() %>% 
    italic(part = "header", j = c(3,4)) %>% 
    align(j = 2:11, align = "center", part = "all") %>% 
    autofit()
  
  kind <- paste("/stats_facets_", model_instrument_version, sep="")
  
  write_txt(summary_facets, kind, model_instrument_version)
  return (facet_table)
}



supp_analyze_factor_loadings <- function(data, model_instrument_version)
{
  o_facets <- data %>% select(starts_with(("of")))
  c_facets <- data %>% select(starts_with(("cf")))
  e_facets <- data %>% select(starts_with(("ef")))
  a_facets <- data %>% select(starts_with(("af")))
  n_facets <- data %>% select(starts_with(("nf")))
  factor_names <- c("O", "C", "E", "A", "N")
  factor_facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
  
  facets_list <- c(names(o_facets), names(c_facets), names(e_facets), 
                   names(a_facets), names(n_facets))
  data_facets <- data %>% 
    select(all_of(facets_list))
  
  # Faktorenanalyse
  faModel <- fa(data_facets, nfactors = 5, rotate = "varimax", fm = "pa", residuals=TRUE)

  sink(paste(supplement_output_folder, "/factor_analysis_", model_instrument_version, ".txt", sep=""))
  print(faModel)
  
  print("Residuals:")
  print(round(faModel$residual, 2))
  print("Eigenvalues:")
  print(round(faModel$values, 2))
  sink()
    
  # Generiere Faktorladungen Tabelle für Word
  loadings_matrix <- faModel$loadings[]
  
  # Convert to data frame and add item names
  loadings_df <- as.data.frame(loadings_matrix)

  loadings_df$h2 = faModel$communality
  loadings_df <- round(loadings_df, 2)  # Round to 2 decimal places

  
  # Add item names as first column (adjust item names as needed)
  loadings_df$Item <- rownames(loadings_matrix)
  loadings_df <- loadings_df %>%
    rowwise() %>% 
    mutate(
      Name = variable_names[[Item]]
    )
  
  loadings_df <- loadings_df[, c("Item", "Name", paste0("PA", 1:5), "h2")]  # Reorder columns
  
  # Row mit Eigenwerten einfügen
  eigenwerte <- round(faModel$values, 2)
  loadings_df <- loadings_df %>% 
    add_row(Item="",
            Name="Eigenwert",
            PA1 = eigenwerte[1],
            PA2 = eigenwerte[2],
            PA3 = eigenwerte[3],
            PA4 = eigenwerte[4],
            PA5 = eigenwerte[5],
            h2 = NULL,
            .before=1)
          
  
  # Tabelle mit fettgedruckten Werten erstellen
  ft <- flextable(loadings_df)
  ft <- ft %>%
    set_header_labels(
      Item = model_instrument_version,
      PA1 = "FA 1",
      PA2 = "FA 2", 
      PA3 = "FA 3",
      PA4 = "FA 4",
      PA5 = "FA 5",
      h2 = "h²"
    ) %>%
    theme_alafoli() %>%
    set_formatter(PA1 = function(x) sapply(x, format_psych)) %>%
    set_formatter(PA2 = function(x) sapply(x, format_psych)) %>%
    set_formatter(PA3 = function(x) sapply(x, format_psych)) %>%
    set_formatter(PA4 = function(x) sapply(x, format_psych)) %>%
    set_formatter(PA5 = function(x) sapply(x, format_psych)) %>%
    set_formatter(h2 = function(x) sapply(x, format_psych)) %>%
    autofit() %>%
    align(align = "center", part = "header") %>%
    align(j = 2:6, align = "center", part = "body") %>%
    bold(i = ~ (PA1 >= 0.4 & PA1 <= 1.0) | PA1 <= -0.4, j = "PA1") %>%
    bold(i = ~ (PA2 >= 0.4 & PA2 <= 1.0) | PA2 <= -0.4, j = "PA2") %>%
    bold(i = ~ (PA3 >= 0.4 & PA3 <= 1.0) | PA3 <= -0.4, j = "PA3") %>%
    bold(i = ~ (PA4 >= 0.4 & PA4 <= 1.0) | PA4 <= -0.4, j = "PA4") %>%
    bold(i = ~ (PA5 >= 0.4 & PA5 <= 1.0) | PA5 <= -0.4, j = "PA5") %>% 
    align(j = 2, align="left") %>% 
    align(j = 7, align="center") %>% 
    italic(i = 1)
  #save_as_docx(ft, path = paste(supplement_output_folder, "/loadings_", model_instrument_version, ".docx", sep=""))
  #write_xlsx(stats, path=paste(supplement_output_folder, kind, ".xlsx",sep=""))
  
  return (ft)
}


supp_analyze_screeplot <- function(data, model_instrument_version)
{
  o_facets <- data %>% select(starts_with(("of")))
  c_facets <- data %>% select(starts_with(("cf")))
  e_facets <- data %>% select(starts_with(("ef")))
  a_facets <- data %>% select(starts_with(("af")))
  n_facets <- data %>% select(starts_with(("nf")))
  factor_names <- c("O", "C", "E", "A", "N")
  factor_facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
  
  facets_list <- c(names(o_facets), names(c_facets), names(e_facets), 
                   names(a_facets), names(n_facets))
  data_facets <- data %>% 
    select(all_of(facets_list))
  
  # Faktorenanalyse
  faModel <- fa(data_facets, nfactors = 5, rotate = "varimax", fm = "pa", residuals=TRUE)
  
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
  ggsave(paste(supplement_output_folder, "/screeplot_" , model_instrument_version, ".jpg", sep=""), plot = screePlot, dpi=300, width = 8, height = 5)
  
  return (screePlot)
}













supp_analyze_factor_correlations <- function(data, model_instrument_version) {
  # analyze factors
  factor_matrix <- data %>% 
    select(ends_with("_llm")) %>% 
    setNames(c("O", "C", "E", "A", "N"))
  matrix <- create_corr_matrix(factor_matrix)
  
  sink(paste(supplement_output_folder, "/factor_correlations_", model_instrument_version, ".txt", sep=""))
  print(matrix)
  sink()
  
  ft <- matrix %>% 
    flextable() %>% 
    theme_alafoli() %>%
    align(align="right", part="all") %>% 
    autofit()
  
  return (ft)
}

supp_analyze_facet_correlations <- function(data, model_instrument_version) {
  facet_matrix <- data %>% 
    select(starts_with(("of")), 
           starts_with(("cf")), 
           starts_with(("ef")),
           starts_with(("af")),
           starts_with(("nf")))
  matrix <- create_corr_matrix(facet_matrix)
  
  sink(paste(supplement_output_folder, "/facet_correlations_", model_instrument_version, ".txt", sep=""))
  print(matrix)
  sink()
  
  ft <- matrix %>% 
    flextable() %>% 
    theme_alafoli() %>% 
    align(align="right", part="all") %>% 
    autofit()
    
  return (ft)  
}



create_corr_matrix <- function(corrdata) {
  
  rcorr_result <- rcorr(as.matrix(corrdata), type = "pearson")

  cor_matrix <- rcorr_result$r
  p_matrix <- rcorr_result$P 
  
  cor_matrix <- apply(cor_matrix, c(1,2), format_psych)
  p_matrix <- apply(p_matrix, c(1,2), format_p_psych)
  
  combined_matrix <- cor_matrix
  combined_matrix[lower.tri(combined_matrix)] <- p_matrix[lower.tri(p_matrix)]
  diag(combined_matrix) <- NA
  
  result_df <- combined_matrix %>% 
    as.data.frame()
  
  # Variablennamen als erste Spalte hinzufügen
  result_df <- result_df %>%
    mutate(Variable = rownames(.), .before = 1) %>% 
    rename(" " = Variable)
  
  return (result_df)
}
