# Model version for printing
model_version <- "v4.1"

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/NEO-PI-R-Names-EN.R")
source("sources/Factor-Names-EN.R")

# get data
data <- tbl(con, "google_analyzation") %>% select(-updated_at) %>%
  filter(essay_id <= 50) %>% 
  collect() %>% 
  drop_na("of1")

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)


# remove Items from analyzation
o_facets <- o_facets[o_facets != "of1"] 
o_facets <- o_facets[o_facets != "of2"]
o_facets <- o_facets[o_facets != "of3"]
o_facets <- o_facets[o_facets != "of4"]

#c_facets <- c_facets[c_facets != "cf2"]
#c_facets <- c_facets[c_facets != "cf5"]
c_facets <- c_facets[c_facets != "cf6"]

#e_facets <- e_facets[e_facets != "ef1"]  
e_facets <- e_facets[e_facets != "ef3"]  
#e_facets <- e_facets[e_facets != "ef6"]  

a_facets <- a_facets[a_facets != "af2"] 
a_facets <- a_facets[a_facets != "af3"]
a_facets <- a_facets[a_facets != "af4"] 
a_facets <- a_facets[a_facets != "af5"] 

n_facets <- n_facets[n_facets != "nf2"]  
n_facets <- n_facets[n_facets != "nf3"] 
n_facets <- n_facets[n_facets != "nf5"]  


all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]

source("sources/aggregate_v4_NEO_temperature.R")

data_facets <- data_facets %>% 
  filter(temperature==0.0) %>% 
  select(all_of(all_facets))

sink(paste("outputs/omega_analyzation_", model_version, ".txt"))
source("sources/omega.R")
sink()


source("sources/macros_v4.R")

# Speziell Analyse Temperatur Faktoren
data_aggregated$temp.factor <- as.factor(data_aggregated$temperature)
plots <- list()
palette <- "Oranges"
alpha <- 0.4

plots[[1]] <- data_aggregated %>% 
  ggplot(aes(x = o_llm, group=temp.factor, fill=temp.factor)) +
  geom_density(alpha = alpha,
                 color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = factor_names[[1]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[2]] <- data_aggregated %>% 
  ggplot(aes(x = c_llm, group=temp.factor, fill=temp.factor)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = factor_names[[2]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[3]] <- data_aggregated %>% 
  ggplot(aes(x = e_llm, group=temp.factor, fill=temp.factor)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = factor_names[[3]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[4]] <- data_aggregated %>% 
  ggplot(aes(x = a_llm, group=temp.factor, fill=temp.factor)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = factor_names[[4]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

plots[[5]] <- data_aggregated %>% 
  ggplot(aes(x = n_llm, group=temp.factor, fill=temp.factor)) +
  geom_density(alpha = alpha,
               color = "black") +
  scale_fill_brewer(type = "qual", palette = palette, guide = "none") +
  labs(title = factor_names[[5]],
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_minimal() 

library(cowplot)
# Legend only
temp_plot <- ggplot(data_aggregated, aes(fill = temp.factor)) +
  geom_density(aes(x = n_llm), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = palette)+
  labs(title = "",
       x = "",
       y = "",
       fill = "Temperatur")
plots[[6]] <- get_legend(temp_plot)

combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] + plot_layout(ncol = 3)
combined_plot

ggsave(paste("graphics/factors_with_tempeature_" , model_version, ".png"), plot = combined_plot, dpi=300, width = 16, height = 10)


# Speziell Essay 14
e14 <- data %>% filter(essay_id ==14, temperature == 1)
plot <- e14 %>% 
  ggplot(aes(x = of6)) +
  geom_histogram(position="dodge",
                 breaks = seq(0.5, 9.5, by = 1),
                 bins = 9, 
                 boundary = 0.5,  # Ensures bins are centered on integers
                 color = "black") +
  scale_fill_brewer(type = "qual", palette = palette) +
  labs(title = "XXX",
       x = "",
       y = "") +
  scale_x_continuous(breaks = 1:9) +
  theme_minimal() 
plot

e14 %>% group_by(temperature) %>% summarise(n=)

