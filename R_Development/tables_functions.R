library(tidyverse)

source("combined_names_EN.R")

# environmental variables:
## tables_output_folder

facets_alpha_omega <- function(data, model_version)
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
  
  omega_o <- omega(data_o, flip=FALSE, plot=FALSE)
  omega_c <- omega(data_c, flip=FALSE, plot=FALSE)
  omega_e <- omega(data_e, flip=FALSE, plot=FALSE)
  omega_a <- omega(data_a, flip=FALSE, plot=FALSE)
  omega_n <- omega(data_n, flip=FALSE, plot=FALSE)
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
  
  omega_results <- tibble(
    factor = c("O", "C", "E", "A", "N", "Mean"),
    alpha = c(
      round(alpha_o$total$raw_alpha, 2),
      round(alpha_c$total$raw_alpha, 2),
      round(alpha_e$total$raw_alpha, 2),
      round(alpha_a$total$raw_alpha, 2),
      round(alpha_n$total$raw_alpha, 2),
      round(mean(c(alpha_o$total$raw_alpha, 
                   alpha_c$total$raw_alpha, 
                   alpha_e$total$raw_alpha,
                   alpha_a$total$raw_alpha,
                   alpha_n$total$raw_alpha)), 2)),
    omega = c(
      round(omega_o$omega.tot, 2),
      round(omega_c$omega.tot, 2),
      round(omega_e$omega.tot, 2),
      round(omega_a$omega.tot, 2),
      round(omega_n$omega.tot, 2),
      round(mean(c(omega_o$omega.tot, 
                   omega_c$omega.tot,
                   omega_e$omega.tot,
                   omega_a$omega.tot,
                   omega_n$omega.tot)),2))
  )

  #print detailed outputs
  sink(paste("outputs/alpha_omega_analyzation_", modelVersion, ".txt"))
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
model_statistics <- function(data, model_version)
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
  
  desc.stats <- data %>% 
    select(all_of(all_facets), all_of(all_factors)) %>% 
    describe()
  desc_df <- desc.stats %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    rowwise() %>%
    mutate(Name = variable_names[Variable],
           ks = suppressWarnings(ks.test(data[[Variable]], "pnorm", 
                        mean(data[[Variable]], na.rm = TRUE), 
                        sd(data[[Variable]], na.rm = TRUE))$p.value),
           sw = shapiro.test(data[[Variable]])$p.value) %>% 
    select(Variable, Name, n, mean, sd, median, min, max, ks, sw) %>%
    mutate(
      across(c(mean, sd, median, min, max), ~round(.x, 2)),
      across(c(ks, sw), ~round(.x, 2))
    )
  psych_table <- desc_df %>%
    flextable() %>%
    set_header_labels(
      Variable = "Variable",
      n = "N",
      mean = "M",
      sd = "SD", 
      median = "Median",
      min = "Min",
      max = "Max",
      ks = "KS-Test",
      sw = "Shapiro-Wilk"
    ) %>%
    theme_vanilla() %>%
    autofit() %>%
    align(j = 2:7, align = "center", part = "all")
  save_as_docx(psych_table, path = paste(tables_output_folder, "/stats_", modelVersion, ".docx", sep=""))
  return (desc_df)
}
