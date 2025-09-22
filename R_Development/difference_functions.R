library(car)
library(effectsize)
library(tidyverse)

source("transformation_functions.R")


create_stats_anova <- function(data, var, group, name) {
  
  formula <- reformulate(group, var)
  
  aov_result = aov(formula, data = data)
  aov_summary <- summary(aov_result)

  f_value <- aov_summary[[1]][1, "F value"]
  p_value <- aov_summary[[1]][1, "Pr(>F)"]
  df_groups <- aov_summary[[1]][1, "Df"]  
  df_measures <- aov_summary[[1]][2, "Df"]
  
  effect_size <- effectsize::eta_squared(aov_result)
  cohens_d <- effectsize::cohens_d(formula(aov_result), data = data)
  
  ################
  residuals <- residuals(aov_result)
  shapiro_test <- shapiro.test(residuals)
  # Levene-Test für Varianzhomogenität
  formula_obj <- formula(aov_result)
  levene_test <- leveneTest(formula_obj, data = data)
  
  return(list(
    name = name,
    p = format_p_psych(p_value),
    F = format_psych(f_value, 3),
    dfg = df_groups,
    dfm = df_measures,
    eta2 = format_psych(effect_size$Eta2, 3),
    d = format_psych(abs(cohens_d$Cohens_d), 3),
    sw = format_p_psych(shapiro_test$p.value),
    lev = format_p_psych(as.numeric(levene_test$`Pr(>F)`[1]))
  ))
}

rFromWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2) 
  r <- z/ sqrt(N)
  return(r)
}


create_stats_h_test <- function(data, var, group, name) {
  
  formula <- reformulate(group, var)
  h_result = kruskal.test(formula, data = data)
  
  eta2 = rank_eta_squared(formula, data = data)$rank_eta_squared
  
  return(list(
    name = name,
    chi2 = format_psych(h_result$statistic,2),
    p = format_p_psych(h_result$p.value),
    eta2 = format_psych(eta2)
  ))
}


create_stats_u_test <- function(data, var, group, name) {
  
  formula <- reformulate(group, var)
  u_result = wilcox.test(formula, data = data)

  N <- nrow(data) # asserting that dataframe only holds test data
  
  return(list(
    name = name,
    W = format_psych(u_result$statistic[["W"]], 0),
    p = format_p_psych(u_result$p.value),
    r = format_psych(rFromWilcox(u_result, N))
  ))
}


#data <- models[["noise"]]
#xx <- create_stats_anova(data, "o_noise", "bin_o", "Noise")
#xx <- create_stats_h_test(data, "o_noise", "bin_o", "Noise")
#xx <- create_stats_u_test(data, "o_noise", "bin_o", "Noise")
