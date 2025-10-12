library(tidyverse)
library(psych)
library(flextable)
library(corrr)
library(Hmisc)

# environmental variables:
## tables_output_folder


create_essay_item_statistics <- function(data, measurement_version, essay_number=NULL) {

  if (is.null(essay_number)) {
    facets <- data %>% 
      select(starts_with(("of")),
            starts_with(("cf")),
            starts_with(("ef")),
            starts_with(("af")),
            starts_with(("nf")))

    essay_number <- "ALL"
  } else {
    facets <- data %>% 
      filter(essay_id == essay_number) %>% 
      select(starts_with(("of")),
             starts_with(("cf")),
             starts_with(("ef")),
             starts_with(("af")),
             starts_with(("nf")))
  }

  stats <- facets %>% 
    psych::describe() %>% 
    round(2)
  ft <- stats %>% 
    as.data.frame() %>% 
    rownames_to_column("Variable") %>%
    flextable() %>% 
    theme_vanilla() %>% 
    autofit()
  save_as_docx(ft, path = paste(tables_output_folder, "/item_raw_statistics_", measurement_version, "_essay_", essay_number, ".docx", sep=""))
}


analyze_alpha_omega <- function(data, measurement_version)
{
  data_o <- data %>% 
    select(starts_with(("of")))
  data_c <- data %>% 
    select(starts_with(("cf")))
  data_e <- data %>% 
    select(starts_with(("ef")))
  data_a <- data %>%
    select(starts_with(("af")))
  data_n <- data %>% 
    select(starts_with(("nf")))
  
  omega_o <- omega(data_o, nfactors = 1, flip=FALSE, plot=FALSE)
  omega_c <- omega(data_c, nfactors = 1, flip=FALSE, plot=FALSE)
  omega_e <- omega(data_e, nfactors = 1, flip=FALSE, plot=FALSE)
  omega_a <- omega(data_a, nfactors = 1, flip=FALSE, plot=FALSE)
  omega_n <- omega(data_n, nfactors = 1, flip=FALSE, plot=FALSE)
  fa_o <- fa(data_o)
  fa_c <- fa(data_c)
  fa_e <- fa(data_e)
  fa_a <- fa(data_n)
  fa_n <- fa(data_o)
  alpha_o <- alpha(data_o)
  alpha_c <- alpha(data_c)
  alpha_e <- alpha(data_e)
  alpha_a <- alpha(data_n)
  alpha_n <- alpha(data_o)
  
  omega_results <- data.frame(
    factor = c("O", "C", "E", "A", "N", "Mittelwert"),
    alpha = c(
      format_psych(alpha_o$total$raw_alpha, 2),
      format_psych(alpha_c$total$raw_alpha, 2),
      format_psych(alpha_e$total$raw_alpha, 2),
      format_psych(alpha_a$total$raw_alpha, 2),
      format_psych(alpha_n$total$raw_alpha, 2),
      format_psych(mean(c(alpha_o$total$raw_alpha, 
                          alpha_c$total$raw_alpha, 
                          alpha_e$total$raw_alpha,
                          alpha_a$total$raw_alpha,
                          alpha_n$total$raw_alpha)), 2)),
    omega = c(
      format_psych(omega_o$omega.tot, 2),
      format_psych(omega_c$omega.tot, 2),
      format_psych(omega_e$omega.tot, 2),
      format_psych(omega_a$omega.tot, 2),
      format_psych(omega_n$omega.tot, 2),
      format_psych(mean(c(omega_o$omega.tot, 
                          omega_c$omega.tot,
                          omega_e$omega.tot,
                          omega_a$omega.tot,
                          omega_n$omega.tot)),2))
  ) %>% 
    setNames(c("Faktor", "\u03B1", "\u03C9"))
  
  ft <- omega_results %>% 
    flextable() %>% 
    theme_vanilla() %>% 
    autofit()
  save_as_docx(ft, path = paste(tables_output_folder, "/alpha_omega_", measurement_version, ".docx", sep=""))
  
  #print detailed outputs
  sink(paste(stats_output_folder, "/alpha_omega_analyzation_", measurement_version, ".txt", sep=""))
  print(omega_results)
  
  print("##### O Factor Analysis")
  print(alpha_o)
  print(omega_o)
  print(fa_o)
  
  print("##### C Factor Analysis")
  print(alpha_c)
  print(omega_c)
  print(fa_c)
  
  print("##### E Factor Analysis")
  print(alpha_e)
  print(omega_e)
  print(fa_e)
  
  print("##### A Factor Analysis")
  print(alpha_a)
  print(omega_a)
  print(fa_a)
  
  print("##### N Factor Analysis")
  print(alpha_n)
  print(omega_n)
  print(fa_n)
  sink()
  
return(omega_results)
}


########################
# descriptive statistics of all facets
analyze_item_statistics <- function(data, measurement_version)
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
  
  sink(paste(stats_output_folder, "/item_statistics_", measurement_version, ".txt", sep=""))
  desc.stats <- data %>% 
    select(all_of(all_facets), all_of(all_factors)) %>% 
    psych::describe()
  print(desc.stats)
    
  # analysiere jede faktorgruppe einzeln
  item_analysis_results <- list()
  
  for(factor in all_factors) {
    factor_items <- data %>% 
      select(starts_with(paste0(substr(factor, 1, 1), "f"))) %>%
      names()
    
    alpha_result <- psych::alpha(data[factor_items])
    print(alpha_result)
    
    item_stats <- alpha_result$item.stats
    item_stats$Variable <- rownames(item_stats)
    item_analysis_results[[factor]] <- item_stats
  }

  # Alle Item-Analyse-Ergebnisse zusammenfassen
  all_item_stats <- do.call(rbind, item_analysis_results)
  
  print(all_item_stats)
  sink()
  
  desc_df <- desc.stats %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    rowwise() %>%
    mutate(Name = variable_names[Variable],
           difficulty = round((((mean-1) / 8)*100)), # assuming scale with 9 steps 
           ks = suppressWarnings(ks.test(data[[Variable]], "pnorm", 
                        mean(data[[Variable]], na.rm = TRUE), 
                        sd(data[[Variable]], na.rm = TRUE))$p.value),
           sw = shapiro.test(data[[Variable]])$p.value) %>% 
    select(Variable, Name, n, mean, sd, median, min, max, ks, sw, difficulty) %>%
    mutate(
      across(c(mean, sd, median, min, max), ~format_psych(.x)),
      across(c(ks, sw), ~format_p_psych(.x))
    )
  
  desc_df <- desc_df %>%
    left_join(
      all_item_stats %>% 
        select(Variable, r.cor, r.drop) %>%
        mutate(
          r.cor = format_psych(r.cor),
          r.drop = format_psych(r.drop)
        ),
      by = "Variable"
    )
  
  # Flextable erstellen
  psych_table <- desc_df %>%
    flextable() %>%
    set_header_labels(
      Variable = "Variable",
      Name = "Name",
      n = "N",
      mean = "M",
      sd = "SD", 
      median = "Median",
      min = "Min",
      max = "Max",
      r.cor = "TS",
      r.drop = "TS/oh",
      difficulty = "IS",
      ks = "KS",
      sw = "SW"
    ) %>%
    theme_vanilla() %>%
    autofit() %>%
    align(j = 3:12, align = "center", part = "all")
  
  save_as_docx(psych_table, path = paste(tables_output_folder, "/stats_", measurement_version, ".docx", sep=""))
  return (desc_df)
}

analyze_factor_loadings <- function(data, measurement_version)
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

  sink(paste(stats_output_folder, "/factor_analysis_", measurement_version, ".txt", sep=""))
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
      Item = measurement_version,
      PA1 = "FA 1",
      PA2 = "FA 2", 
      PA3 = "FA 3",
      PA4 = "FA 4",
      PA5 = "FA 5",
      h2 = "h²"
    ) %>%
    theme_vanilla() %>%
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
  save_as_docx(ft, path = paste(tables_output_folder, "/loadings_", measurement_version, ".docx", sep=""))

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
  ggsave(paste(graphics_output_folder, "/screeplot_" , measurement_version, ".png", sep=""), plot = screePlot, dpi=300, width = 8, height = 5)
}

analyze_correlations <- function(data, measurement_version) {
  # analyze factors
  factor_matrix <- data %>% 
    select(ends_with("_llm")) %>% 
    setNames(c("O", "C", "E", "A", "N"))
  matrix <- create_corr_matrix(factor_matrix)
  ft <- matrix %>% 
    flextable() %>% 
    theme_vanilla() %>%
    align(align="right", part="all") %>% 
    autofit()
  save_as_docx(ft, path = paste(tables_output_folder, "/corrs_factors_", measurement_version, ".docx", sep=""))

  facet_matrix <- data %>% 
    select(starts_with(("of")), 
           starts_with(("cf")), 
           starts_with(("ef")),
           starts_with(("af")),
           starts_with(("nf")))
  matrix <- create_corr_matrix(facet_matrix)
  ft <- matrix %>% 
    flextable() %>% 
    theme_vanilla() %>% 
    align(align="right", part="all") %>% 
    autofit()
  save_as_docx(ft, path = paste(tables_output_folder, "/corrs_facets_", measurement_version, ".docx", sep=""))
  
  
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







# UNUSED !!! XXX
create_factor_corr_values <- function(data){
  all_factors <- data %>% select(
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")) %>% names()
  
  all_bins <- data %>% 
    select(starts_with("bin_")) %>%
    names()
  
  result <- numeric(0)  # Changed from list() to numeric vector
  
  for(i in 1:5) {
    corresult <- rcorr(data[[all_factors[i]]], data[[all_bins[i]]])
    
    result <- c(result, corresult$r[2, 1])  # Changed indexing
  }
  
  meanc <- mean(result)
  result <- c(result, meanc)
  names(result) <- c("O", "C", "E", "A", "N", "M")
  
  return(result)
}
