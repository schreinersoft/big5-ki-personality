library("tidyverse")
library("patchwork")

source("combined_names_EN.R")

# environmental variables:
## graphics_output_folder
## tables_output_folder

# create all histogram plots of raw data for one essay
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
    paste(graphics_output_folder,"/histograms_", variable_names[[factor_names[[j]]]], "_", model_version, "essay_", essay_number, ".png", sep = ""),
    plot = combined_plot, dpi=300, width = plot_width, height = plot_height)
  j <- j + 1
  }
}


# create facet density plots of all essays
create_facet_densities <- function(data)
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
    facets_df <- factor_facets_list[[i]]  # This is a data frame
    plots <- list()  # Reset plots list for each factor
    
    # Create plots for all facets of current factor
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
          args = list(mean = mean(d[[facet]], na.rm = TRUE), 
                      sd = sd(d[[facet]], na.rm = TRUE)), 
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
    ggsave(paste(graphics_output_folder,"/density_", factor_name, "_", modelVersion, "_facets.png", sep = ""), 
           plot = combined_plot, dpi = 300, width = 8, height = 6)
  }
}

