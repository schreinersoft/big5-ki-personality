library(tidyverse)
library(rstatix)

source("connect_database.R")
source("functions.R")
source("Factor-Names-EN.R")

essays <- tbl(con, "essays")  %>% select(-text, -author) %>% collect()
minej <- tbl(con, "minej_analyzation")  %>% select(-updated_at,) %>% collect
minej_joined <- left_join(essays, minej, by = c("id" = "essay_id")) %>% 
  drop_na(o_minej)
minej_truncated <- minej_joined %>% filter(classification_type == "truncated")
minej_sliding <- minej_joined %>% filter(classification_type == "slidingWindow")

# suboptimal!
verteilung_factornames_minej <- function(data, variable, group=NULL) {
  mean_val <- data %>% 
    pull(!!sym(variable)) %>% 
    mean(na.rm = TRUE)
  sd_val <- data %>% 
    pull(!!sym(variable)) %>% 
    sd(na.rm = TRUE)
  
  data %>% 
    ggplot(aes(x = !!sym(variable))) +
    geom_density(color = "black",
                 fill = "lightblue") +   # XXX factor colors?
    labs(title = factor_names[variable],
         #x = "Value",
         y = "") +
    stat_function(
      fun = dnorm,  # Normal distribution function
      args = list(mean = mean_val, sd = sd_val), 
      color = "blue", linewidth = 0.5, linetype = "dashed"
    ) +
    theme_minimal() 
}

data <- minej_truncated


# set up facets
o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facets_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
#all_factors <- c("O", "C", "E", "A", "N")
all_factors <- c("o_minej_z", "c_minej_z", "e_minej_z", "a_minej_z", "n_minej_z")
all_factor_names <- factor_names[all_factors]


# z-Normalisierung
data$o_minej_z = as.numeric(scale(data$o_minej))
data$c_minej_z = as.numeric(scale(data$c_minej))
data$e_minej_z = as.numeric(scale(data$e_minej))
data$a_minej_z = as.numeric(scale(data$a_minej))
data$n_minej_z = as.numeric(scale(data$n_minej))
data$o_bin <- ifelse(data$o_binary == "1", 1, 0)
data$c_bin <- ifelse(data$c_binary == "1", 1, 0)
data$e_bin <- ifelse(data$e_binary == "1", 1, 0)
data$a_bin <- ifelse(data$a_binary == "1", 1, 0)
data$n_bin <- ifelse(data$n_binary == "1", 1, 0)
data$o_bin_z <- ifelse(data$o_binary == "1", 1, -1)
data$c_bin_z <- ifelse(data$c_binary == "1", 1, -1)
data$e_bin_z <- ifelse(data$e_binary == "1", 1, -1)
data$a_bin_z <- ifelse(data$a_binary == "1", 1, -1)
data$n_bin_z <- ifelse(data$n_binary == "1", 1, -1)

### create a data_temp score
data <- data %>% 
  rowwise() %>% 
  mutate(
    o_minej_z_prod = o_minej_z * o_bin_z,
    c_minej_z_prod = c_minej_z * c_bin_z,
    e_minej_z_prod = e_minej_z * e_bin_z,
    a_minej_z_prod = a_minej_z * a_bin_z,
    n_minej_z_prod = n_minej_z * n_bin_z,
    minej_sim_score = sum(across(ends_with("z_prod")))
  )

# DIES wäre der Kennwert
mean(data$minej_sim_score)



# analyze OCEAN factors of all essays
plots <- list()
i <- 1
for (factor in all_factors){
  plots[[i]] <- data_temp %>%
    verteilung_factornames_minej(factor)
  i <- i + 1
}
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plot_layout(ncol = 3)
combined_plot
ggsave(paste("graphics/density_", modelVersion, "_temp", temp, "_factors.png"), plot = combined_plot, dpi=300, width = 8, height = 6)


# descriptive statistics of all facets
desc.stats <- data_temp %>% 
  select(all_of(all_factors)) %>% 
  describe()
desc_df <- desc.stats %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  select(Variable, n, mean, sd, median, min, max) %>%
  mutate(
    across(c(mean, sd, median, min, max), ~round(.x, 2))
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
    max = "Max"
  ) %>%
  theme_vanilla() %>%
  autofit() %>%
  align(j = 2:7, align = "center", part = "all")
save_as_docx(psych_table, path = paste("tables/desc_minej_factors.docx")) 

sink("outputs/analyze_desc_minej_factors.txt")
ks.tests <- list()
i <- 1
# Kolmogorov-Smirnov-Tests
for (factor in all_factors){
  vec <- data_temp %>%
    select(!!sym(factor)) %>% 
    pull(!!sym(factor))
  ks.tests[[i]] <- ks.test(vec, "pnorm", mean(vec), sd(vec))
  i <- i+1
}
print(ks.tests)
sink()


plot0 <- data_temp %>%
  filter(o_binary == 0) %>% 
  z_verteilung_title("o_minej_z", "Openness 0", 3.0)
plot1<- data_temp %>%
  filter(o_binary == 1) %>% 
  z_verteilung_title("o_minej_z", "Openness 1", 3.0)

plot0 + plot1


plot0 <- data_temp %>%
  filter(c_binary == 0) %>% 
  z_verteilung_title("c_minej_z", "Conscentiousness 0", 3.0)
plot1<- data_temp %>%
  filter(c_binary == 1) %>% 
  z_verteilung_title("c_minej_z", "Conscentiousness 1", 3.0)

plot0 + plot1


plot0 <- data_temp %>%
  filter(e_binary == 0) %>% 
  z_verteilung_title("e_minej_z", "Extraversion 0", 3.0)
plot1<- data_temp %>%
  filter(e_binary == 1) %>% 
  z_verteilung_title("e_minej_z", "Extraversion 1", 3.0)

plot0 + plot1


plot0 <- data_temp %>%
  filter(a_binary == 0) %>% 
  z_verteilung_title("a_minej_z", "Agreeableness 0", 3.0)
plot1<- data_temp %>%
  filter(a_binary == 1) %>% 
  z_verteilung_title("a_minej_z", "Agreeableness 1", 3.0)

plot0 + plot1


plot0 <- data_temp %>%
  filter(n_binary == 0) %>% 
  z_verteilung_title("n_minej_z", "Neuroticism 0", 3.0)
plot1<- data_temp %>%
  filter(n_binary == 1) %>% 
  z_verteilung_title("n_minej_z", "Neuroticism 1", 3.0)

plot0 + plot1


# Boxplots
data %>% 
  boxplot("o_minej_z", "o_binary")
data %>% 
  boxplot("c_minej_z", "c_binary")
data %>% 
  boxplot("e_minej_z", "e_binary")
data %>% 
  boxplot("a_minej_z", "a_binary")
data %>% 
  boxplot("n_minej_z", "n_binary")


# ANOVA der Binärgruppen
# --> truncated alle tests signifikant!
model_oneway <- aov(o_minej_z ~ o_binary, data = data)
summary(model_oneway)
model_oneway <- aov(c_minej_z ~ c_binary, data = data)
summary(model_oneway)
model_oneway <- aov(e_minej_z ~ e_binary, data = data)
summary(model_oneway)
model_oneway <- aov(a_minej_z ~ a_binary, data = data)
summary(model_oneway)
model_oneway <- aov(n_minej_z ~ n_binary, data = data)
summary(model_oneway)









############################### OLD


# ANOVA der Binärgruppen
# --> truncated alle tests signifikant!
model_oneway <- aov(o_minej ~ o_binary, data = data)
summary(model_oneway)
model_oneway <- aov(o_minej ~ o_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(c_minej ~ c_binary, data = data)
summary(model_oneway)
model_oneway <- aov(c_minej ~ c_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(e_minej ~ e_binary, data = data)
summary(model_oneway)
model_oneway <- aov(e_minej ~ e_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(a_minej ~ a_binary, data = data)
summary(model_oneway)
model_oneway <- aov(a_minej ~ a_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(n_minej ~ n_binary, data = data)
summary(model_oneway)
model_oneway <- aov(n_minej ~ n_binary, data = minej_sliding)
summary(model_oneway)

# U-Tests
# 1. Deskriptive Statistiken
desc_stats <- data %>%
  group_by(o_binary) %>%
  collect() %>% 
  summarise(
    n = n(),
    median = median(o_minej),
    iqr = IQR(o_minej),
    mean_rank = mean(rank(data$o_minej)[data$o_binary == cur_group()[[1]]])
  )
print(desc_stats)

# 2. Wilcoxon-Test
# --> ebenfalls alle signifikant!
wilcox_result <- wilcox.test(o_minej ~ o_binary, data = data)
print(wilcox_result)
wilcox_result <- wilcox.test(c_minej ~ c_binary, data = data)
print(wilcox_result)
wilcox_result <- wilcox.test(e_minej ~ e_binary, data = data)
print(wilcox_result)
wilcox_result <- wilcox.test(a_minej ~ a_binary, data = data)
print(wilcox_result)
wilcox_result <- wilcox.test(n_minej ~ n_binary, data = data)
print(wilcox_result)

# 3. Effektgröße (r)
n <- nrow(data)
z_score <- qnorm(wilcox_result$p.value / 2)  # Z-Wert aus p-Wert
r <- abs(z_score) / sqrt(n)  # Effektgröße r

cat("Effektgröße r:", round(r, 3), "\n")

# Interpretation der Effektgröße
if (r < 0.3) {
  cat("Kleiner Effekt\n")
} else if (r < 0.5) {
  cat("Mittlerer Effekt\n")
} else {
  cat("Großer Effekt\n")
}

# 4. Interpretation
cat("\nInterpretation:\n")
if(wilcox_result$p.value < 0.05) {
  cat("Signifikanter Unterschied zwischen den Gruppen (p =", 
      round(wilcox_result$p.value, 4), ")\n")
} else {
  cat("Kein signifikanter Unterschied zwischen den Gruppen (p =", 
      round(wilcox_result$p.value, 4), ")\n")
}





# Vergleiche nach Binärvariable
# Verteilungen
# --> C schief
data %>% 
  verteilung("o_minej", "o_binary")  
data %>% 
  verteilung("c_minej", "c_binary")  
data %>% 
  verteilung("e_minej", "e_binary")  
data %>% 
  verteilung("a_minej", "a_binary")  
data %>% 
  verteilung("n_minej", "n_binary")  

# Einfacher Q-Q Plot
data %>% 
  ggplot(aes(sample = e_minej)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", x = "Theoretische Quantile", y = "Stichproben-Quantile")


# Violins
minej_joined %>% 
  violinJitter("o_minej", "o_binary")  
minej_joined %>% 
  violinJitter("c_minej", "c_binary")  
minej_joined %>% 
  violinJitter("e_minej", "e_binary")  
minej_joined %>% 
  violinJitter("a_minej", "a_binary")  
minej_joined %>% 
  violinJitter("n_minej", "n_binary")  

# Boxplots
minej_joined %>% 
  boxplot("o_minej", "o_binary")
minej_joined %>% 
  boxplot("c_minej", "c_binary")
minej_joined %>% 
  boxplot("e_minej", "e_binary")
minej_joined %>% 
  boxplot("a_minej", "a_binary")
minej_joined %>% 
  boxplot("n_minej", "n_binary")

# Histogramme
minej_joined %>% 
  histogramm("o_minej", "o_binary")
minej_joined %>% 
  histogramm("c_minej", "c_binary")
minej_joined %>% 
  histogramm("e_minej", "e_binary")
minej_joined %>% 
  histogramm("a_minej", "a_binary")
minej_joined %>% 
  histogramm("n_minej", "n_binary")


# OCEAN gesamt
ocean <- minej_joined %>%
  select(o_minej, c_minej, e_minej, a_minej, n_minej) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")

ocean %>% 
  ggplot(aes(x = value, color = variable, fill = variable)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Curves for OpenAI Variables",
       x = "Value",
       y = "Density",
       color = "Variable",
       fill = "Variable") +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2")

# Multi View
minej_joined %>% 
  verteilung_multi(c("o_minej", "c_minej", "e_minej", "a_minej", "n_minej"))

minej_ausreisser <- minej_joined %>% filter(n_minej < 3 && n_binary == "0")
minej_ausreisser  

# Numerische Statistiken
minej_joined %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_minej),
    sd = sd(n_minej),
    n=n()
  )

data %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_minej),
    sd = sd(n_minej),
    n=n()
  )

minej_sliding %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_minej),
    sd = sd(n_minej),
    n=n()
  )

minej_joined %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_minej),
    sd = sd(a_minej),
    n=n()
  )

minej_joined %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_minej),
    sd = sd(n_minej),
    n=n()
  )


