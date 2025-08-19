library(tidyverse)

verteilung <- function(data, variable, group=NULL) {
  if (is.null(group)) {
    data %>% 
      ggplot(aes(x = !!sym(variable))) +
      geom_density(fill="green") +
      labs(title = "Density",
           x = "Value",
           y = "Density") +
      theme_minimal() 
  }
  else {
    data %>% 
      ggplot(aes(x = !!sym(variable), fill=!!sym(group))) +
      geom_density(alpha=0.5) +
      labs(title = "Density",
           x = "Value",
           y = "Density") +
      theme_minimal() 
  }
}
violinJitter <- function(data, variable, group=NULL) {
  if (is.null(group)) {
    data %>% 
      ggplot(aes(x = !!sym(variable), y = !!sym(variable))) +
      geom_violin(fill="red") +
      labs(title = "Density",
           x = "Value",
           y = "Density") +
      theme_minimal() 
  }
  else {
    data %>% 
      ggplot(aes(x = !!sym(group), y=!!sym(variable), fill=!!sym(group))) +
      geom_violin() +
      geom_jitter(width=0.1,alpha=0.5)+
      labs(title = "Density",
           x = "Value",
           y = "Density") +
      theme_minimal() 
  }
}

verteilung_multi <- function(data, variables) {
  data %>% 
    select(all_of(variables)) %>% 
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
    ggplot(aes(x = value)) +
    geom_density(fill = "blue", alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Verteilungen OCEAN",
         x = "Value",
         y = "Density") +
    theme_minimal()
}


histogramm <- function(data, variable, group=NULL) {
  if (is.null(group)) {
    data %>% 
      ggplot(aes(x = !!sym(variable), fill="green")) +
      geom_histogram() +
      labs(title = "Histogramm",
           x = "Value",
           y = "Density") +
      theme_minimal() 
  }
  else {
    data %>% 
      ggplot(aes(x = !!sym(variable), fill=!!sym(group))) +
      geom_histogram(position="dodge") +
      labs(title = "Histogramm",
           x = "Value",
           y = "Density") +
      theme_minimal() 
  }
}

histogramm_multi <- function(data, variables) {
  data %>% 
    select(all_of(variables)) %>% 
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
    ggplot(aes(x = value, color = variable, fill = variable)) +
    geom_histogram(position="dodge") +
    labs(title = "Verteilungen OCEAN",
         x = "Value",
         y = "Density") +
    theme_minimal()
}


boxplot <- function(data, variable, group=NULL) {
  if (is.null(group)) {
    data %>%
      ggplot(aes(y = !!sym(variable))) +
      geom_boxplot() +
      labs(title = "Boxplot") +
      theme_minimal()
  }
  else {
    data %>%
      ggplot(aes(x = !!sym(group), y = !!sym(variable), group = !!sym(group))) +
      geom_boxplot() +
      labs(title = "Boxplots",
           x = "Groups", y = "Values") +
      theme_minimal()
  }
}
