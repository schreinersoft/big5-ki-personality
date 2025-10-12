# analyze OCEAN factors of all essays
plots <- list()
i <- 1
for (factor in all_factors){
  plots[[i]] <- data_aggregated %>%
    verteilung_factornames(factor)
  i <- i + 1
}
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plot_layout(ncol = 3)
combined_plot
ggsave(paste("graphics/density_", measurement_version, "_factors.png"), plot = combined_plot, dpi=300, width = 8, height = 6)
