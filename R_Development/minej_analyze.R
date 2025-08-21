library(tidyverse)
library(rstatix)

source("connect_minej_truncatedbase.R")
source("functions.R")

essays <- tbl(con, "essays")  %>% select(-text, -author) %>% collect()
minej <- tbl(con, "minej_analyzation")  %>% select(-updated_at,) %>% collect
minej_joined <- left_join(essays, minej, by = c("id" = "essay_id")) %>% 
  drop_na(o_minej)
minej_truncated <- minej_joined %>% filter(classification_type == "truncated")
minej_sliding <- minej_joined %>% filter(classification_type == "slidingWindow")


# z-Normalisierung
minej_truncated$o_minej_z = as_vector(scale(minej_truncated$o_minej))
minej_truncated$c_minej_z = as_vector(scale(minej_truncated$c_minej))
minej_truncated$e_minej_z = as_vector(scale(minej_truncated$e_minej))
minej_truncated$a_minej_z = as_vector(scale(minej_truncated$a_minej))
minej_truncated$n_minej_z = as_vector(scale(minej_truncated$n_minej))

# Tests auf Normalverteilung
# --> nur A und E normalverteilt
vec <- minej_truncated %>%
  select(o_minej) %>% 
  pull(o_minej)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- minej_truncated %>%
  select(c_minej) %>% 
  pull(c_minej)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- minej_truncated %>%
  select(e_minej) %>% 
  pull(e_minej)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- minej_truncated %>%
  select(a_minej) %>% 
  pull(a_minej)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- minej_truncated %>%
  select(n_minej) %>% 
  pull(n_minej)
ks.test(vec, "pnorm", mean(vec), sd(vec))

# ANOVA der Binärgruppen
# --> truncated alle tests signifikant!
model_oneway <- aov(o_minej ~ o_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(o_minej ~ o_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(c_minej ~ c_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(c_minej ~ c_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(e_minej ~ e_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(e_minej ~ e_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(a_minej ~ a_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(a_minej ~ a_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(n_minej ~ n_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(n_minej ~ n_binary, data = minej_sliding)
summary(model_oneway)

# U-Tests
# 1. Deskriptive Statistiken
desc_stats <- minej_truncated %>%
  group_by(o_binary) %>%
  collect() %>% 
  summarise(
    n = n(),
    median = median(o_minej),
    iqr = IQR(o_minej),
    mean_rank = mean(rank(minej_truncated$o_minej)[minej_truncated$o_binary == cur_group()[[1]]])
  )
print(desc_stats)

# 2. Wilcoxon-Test
# --> ebenfalls alle signifikant!
wilcox_result <- wilcox.test(o_minej ~ o_binary, data = minej_truncated)
print(wilcox_result)
wilcox_result <- wilcox.test(c_minej ~ c_binary, data = minej_truncated)
print(wilcox_result)
wilcox_result <- wilcox.test(e_minej ~ e_binary, data = minej_truncated)
print(wilcox_result)
wilcox_result <- wilcox.test(a_minej ~ a_binary, data = minej_truncated)
print(wilcox_result)
wilcox_result <- wilcox.test(n_minej ~ n_binary, data = minej_truncated)
print(wilcox_result)

# 3. Effektgröße (r)
n <- nrow(minej_truncated)
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
minej_truncated %>% 
  verteilung("o_minej", "o_binary")  
minej_truncated %>% 
  verteilung("c_minej", "c_binary")  
minej_truncated %>% 
  verteilung("e_minej", "e_binary")  
minej_truncated %>% 
  verteilung("a_minej", "a_binary")  
minej_truncated %>% 
  verteilung("n_minej", "n_binary")  

# Einfacher Q-Q Plot
minej_truncated %>% 
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

minej_truncated %>%
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


