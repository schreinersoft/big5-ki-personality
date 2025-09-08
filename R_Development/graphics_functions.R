library(tidyverse)
library(psych)
library(lavaan)
library(knitr)
library(semTools)
library(corrplot)
library(patchwork)
library(purrr)
library(flextable)

source("combined_names_EN.R")

# environmental variables:
## graphics_output_folder
## tables_output_folder
## raw_output_folder

# create all histogram plots of raw data for one essay
# needs raw_data, not aggregated!
create_essay_histograms <- function(data, model_version, essay_number)
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

  for (i in 1:length(factor_names)) {
    factor_name <- factor_names[i]
    facets_df <- factor_facets_list[[i]]  # This is a data frame
    plots <- list()  # Reset plots list for each factor
    
    # Create plots for all facets of current factor
    plot_index <- 1
    for (facet in names(facets_df)) {  # Iterate through column names
      plots[[plot_index]] <- data %>%
        filter(essay_id == essay_number) %>% 
        ggplot(aes(x = .data[[facet]])) +
        geom_histogram(breaks = seq(0.5, 9.5, by = 1),
                       bins = 9, 
                       boundary = 0.5,  # Ensures bins are centered on integers
                       color = "black", fill = "lightblue") +
        labs(title = variable_names[facet],
             #x = "Wert",
             y = "") +
        scale_x_continuous(breaks = 1:9) +
        theme_minimal() 
      
      plot_index <- plot_index + 1
    }
    
    # Calculate layout dimensions after all plots are created
    n_plots <- length(plots)
    n_cols <- min(n_plots, 3)
    n_rows <- ceiling(n_plots / n_cols)
    
    # Create and save combined plot for this factor
    combined_plot <- wrap_plots(plots, ncol = n_cols) + plot_annotation()
    
  ggsave(
    paste(graphics_output_folder,"/histograms_", variable_names[[factor_names[[i]]]], "_", model_version, "essay_", essay_number, ".png", sep = ""),
    plot = combined_plot, dpi=300, width = n_cols * 3, height = n_rows * 3)
  }
}


# create facet density plots of all essays
create_facet_densities <- function(data, model_version)
{
  o_facets <- data %>% select(starts_with(("of")))
  c_facets <- data %>% select(starts_with(("cf")))
  e_facets <- data %>% select(starts_with(("ef")))
  a_facets <- data %>% select(starts_with(("af")))
  n_facets <- data %>% select(starts_with(("nf")))
  factor_names <- c("O", "C", "E", "A", "N")
  factor_facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)

  for (i in 1:length(factor_names)) {
    factor_name <- factor_names[i]
    facets_df <- factor_facets_list[[i]]
    plots <- list()
    
    plot_index <- 1
    
    for (facet in names(facets_df)) { 
      plots[[plot_index]] <- data %>%
        ggplot(aes(x = .data[[facet]])) +
        xlim(1, 9) +
        geom_density(color = "black",
                     fill = "lightblue") +
        labs(title = variable_names[facet],
             y = "") +
        stat_function(
          fun = dnorm,
          args = list(mean = mean(data[[facet]], na.rm = TRUE), 
                      sd = sd(data[[facet]], na.rm = TRUE)), 
          color = "blue", linewidth = 0.5, linetype = "dashed"
        ) +
        theme_minimal()
      
      plot_index <- plot_index + 1
    }
    
    # Calculate layout dimensions after all plots are created
    n_plots <- length(plots)
    n_cols <- min(n_plots, 3)
    n_rows <- ceiling(n_plots / n_cols)
    
    # Create and save combined plot for this factor
    combined_plot <- wrap_plots(plots, ncol = n_cols)
    ggsave(paste(graphics_output_folder,"/density_", model_version, "_", factor_name, "_facets.png", sep = ""), 
           plot = combined_plot, dpi = 300, width = n_cols * 3, height = n_rows * 3)
  }
}

# plot OCEAN factors of all essays
create_factor_densities <- function(data, model_version)
{
  all_factors <- data %>% select(
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")) %>% names()
  
  plots <- list()
  i <- 1
  for (factor in all_factors){
    plots[[i]] <- data %>%
      ggplot(aes(x = .data[[factor]])) +
      xlim(1, 9) +
      geom_density(color = "black",
                   fill = "orange") +   # XXX factor colors?
      labs(title = variable_names[[factor]] %||% factor,
           #x = "Value",
           y = "") +
      stat_function(
        fun = dnorm,  # Normal distribution function
        args = list(mean = mean(data[[factor]], na.rm = TRUE), 
                    sd = sd(data[[factor]], na.rm = TRUE)), 
        color = "blue", linewidth = 0.5, linetype = "dashed"
      ) +
      theme_minimal() 
    i <- i + 1
  }
  
  # Calculate layout dimensions after all plots are created
  n_plots <- length(plots)
  n_cols <- min(n_plots, 3)
  n_rows <- ceiling(n_plots / n_cols)
  
  combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plot_layout(ncol = 3)
  combined_plot
  ggsave(paste(graphics_output_folder, "/density_", model_version, "_factors.png", sep=""),
         plot = combined_plot, dpi=300, width = n_cols * 3, height = n_rows * 3)
}

# Correlation matrices
create_correlation_matrices <- function(data, model_version)
{
  data_facets <- data %>%
    select(starts_with(("of")),
           starts_with(("cf")),
           starts_with(("ef")),
           starts_with(("af")),
           starts_with(("nf")))
  
  data_factors <- data %>%
    select(starts_with(("o_")),
           starts_with(("c_")),
           starts_with(("e_")),
           starts_with(("a_")),
           starts_with(("n_")))

  sink(paste(stats_output_folder, "/output_analyzation_", model_version, ".txt", sep=""))

  cor_matrix <- corr.test(data_facets, use = "complete.obs", method="pearson")
  cor_matrix_rounded <- round(cor_matrix$r, 2)
  print("Correlation Matrix (Pearson's r):")
  print(cor_matrix_rounded)
  print("p-Values:")
  print(round(cor_matrix$p, 3))

  
  png(paste(graphics_output_folder, "/corrplot_", model_version, "_facets.png", sep=""), width = 2400, height = 2400, res = 150)
  corrplot(cor_matrix_rounded, method = "color", type = "upper", 
           addCoef.col = "black", tl.cex = 0.8)
  dev.off()
  
  # Facets Spearman
  cor_matrix <- corr.test(data_facets, use = "complete.obs", method="spearman")
  # Round to 2 decimal places
  cor_matrix_rounded <- round(cor_matrix$r, 2)
  print("Correlation Matrix (Spearmans's rho):")
  print(cor_matrix_rounded)
  print("p-Values:")
  print(round(cor_matrix$p, 3))
  
  # Plot correlation Matrix
  png(paste(graphics_output_folder, "/corrplot_spearman_", model_version, "_facets.png", sep=""), width = 2400, height = 2400, res = 150)
  corrplot(cor_matrix_rounded, method = "color", type = "upper", 
           addCoef.col = "black", tl.cex = 0.8)
  dev.off()
  
  # Factors
  cor_matrix <- corr.test(data_factors, use = "complete.obs", method="pearson")
  # Round to 2 decimal places
  cor_matrix_rounded <- round(cor_matrix$r, 2)
  print("Correlation Matrix (Pearson's r):")
  print(cor_matrix_rounded)
  print("p-Values:")
  print(round(cor_matrix$p, 3))
  
  # Plot correlation Matrix
  png(paste(graphics_output_folder, "/corrplot_factors_", model_version, ".png", sep=""), width = 2400, height = 2400, res = 150)
  corrplot(cor_matrix_rounded, method = "color", type = "upper", 
           addCoef.col = "black", tl.cex = 0.8)
  dev.off()
  sink()
}

