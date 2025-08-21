library(tidyverse)
library(patchwork)
library(purrr)

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
      ggplot(aes(x = !!sym(variable))) +
      geom_histogram(breaks = seq(0.5, 9.5, by = 1),
                     bins = 9, 
                     boundary = 0.5,  # Ensures bins are centered on integers
                     color = "black", fill = "lightblue") +
      labs(title = "Histogramm",
           x = "Wert",
           y = "Anzahl") +
      scale_x_continuous(breaks = 1:9) +
      theme_minimal() 
  }
  else {
    data %>% 
      ggplot(aes(x = !!sym(variable), fill=!!sym(group))) +
      geom_histogram(breaks = seq(0.5, 9.5, by = 1),
                     bins = 9, 
                     boundary = 0.5,  # Ensures bins are centered on integers
                     color = "black", fill = "lightblue") +
      labs(title = "Histogramm",
           x = "Wert",
           y = "Anzahl") +
      scale_x_continuous(breaks = 1:9) +
      theme_minimal() 
  }
}

histogramm_sechsfach <- function(data, facets) {
  # Liste für Plots initialisieren
  p <- list()
  
  # Plots erstellen
  for (i in 1:length(facets)) {
    p[[i]] <- histogramm(data, facets[i])
  }
  
  # 3x2 Matrix erstellen
  return((p[[1]] | p[[2]] | p[[3]]) / (p[[4]] | p[[5]] | p[[6]]))
}

histogramm_multi <- function(data, variables) {
  data %>% 
    select(all_of(variables)) %>% 
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
    ggplot(aes(x = value, color = variable, fill = variable)) +
    geom_histogram(position="dodge",breaks = seq(0.5, 9.5, by = 1),
                   bins = 9, 
                   boundary = 0.5) +  # Ensures bins are centered on integers) +
    labs(title = "Verteilungen OCEAN",
         x = "Value",
         y = "Density") +
    scale_x_continuous(breaks = 1:9) +
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
