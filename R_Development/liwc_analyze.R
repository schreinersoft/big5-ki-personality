library(tidyverse)
library(rstatix)

source("connect_database.R")
source("functions.R")

essays <- tbl(con, "essays")  %>% select(-text, -author) %>% collect()
liwc <- tbl(con, "liwc_analyzation")  %>% select(-updated_at, -liwc_all) %>% collect
liwc_data <- left_join(essays, liwc, by = c("id" = "essay_id")) %>% 
  drop_na(o_liwc)


# z-Normalisierung
liwc_data$o_liwc_z = as_vector(scale(liwc_data$o_liwc))
liwc_data$c_liwc_z = as_vector(scale(liwc_data$c_liwc))
liwc_data$e_liwc_z = as_vector(scale(liwc_data$e_liwc))
liwc_data$a_liwc_z = as_vector(scale(liwc_data$a_liwc))
liwc_data$n_liwc_z = as_vector(scale(liwc_data$n_liwc))

liwc_data$o_bin_z = ifelse(liwc_data$o_binary == "1", 1, -1)
liwc_data$c_bin_z = ifelse(liwc_data$c_binary == "1", 1, -1)
liwc_data$e_bin_z = ifelse(liwc_data$e_binary == "1", 1, -1)
liwc_data$a_bin_z = ifelse(liwc_data$a_binary == "1", 1, -1)
liwc_data$n_bin_z = ifelse(liwc_data$n_binary == "1", 1, -1)

# Tests auf Normalverteilung
# --> alle normalverteilt!
vec <- liwc_data %>%
  select(o_liwc) %>% 
  pull(o_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(c_liwc) %>% 
  pull(c_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(e_liwc) %>% 
  pull(e_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(a_liwc) %>% 
  pull(a_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(n_liwc) %>% 
  pull(n_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))


# ANOVA der Binärgruppen
model_oneway <- aov(o_liwc ~ o_binary, data = liwc_data)
summary(model_oneway)
model_oneway <- aov(c_liwc ~ c_binary, data = liwc_data)
summary(model_oneway)
model_oneway <- aov(e_liwc ~ e_binary, data = liwc_data)
summary(model_oneway)
model_oneway <- aov(a_liwc ~ a_binary, data = liwc_data)
summary(model_oneway)
model_oneway <- aov(n_liwc ~ n_binary, data = liwc_data)
summary(model_oneway)

# U-Tests
# 1. Deskriptive Statistiken
desc_stats <- data %>%
  group_by(o_binary) %>%
  collect() %>% 
  summarise(
    n = n(),
    median = median(o_liwc),
    iqr = IQR(o_liwc),
    mean_rank = mean(rank(data$o_liwc)[data$o_binary == cur_group()[[1]]])
  )
print(desc_stats)

# 2. Wilcoxon-Test
wilcox_result <- wilcox.test(o_liwc ~ o_binary, data = liwc_data)
print(wilcox_result)
wilcox_result <- wilcox.test(c_liwc ~ c_binary, data = liwc_data)
print(wilcox_result)
wilcox_result <- wilcox.test(e_liwc ~ e_binary, data = liwc_data)
print(wilcox_result)
wilcox_result <- wilcox.test(a_liwc ~ a_binary, data = liwc_data)
print(wilcox_result)
wilcox_result <- wilcox.test(n_liwc ~ n_binary, data = liwc_data)
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
liwc_data %>% 
  verteilung("o_liwc", "o_binary")  
liwc_data %>% 
  verteilung("c_liwc", "c_binary")  
liwc_data %>% 
  verteilung("e_liwc", "e_binary")  
liwc_data %>% 
  verteilung("a_liwc", "a_binary")  
liwc_data %>% 
  verteilung("n_liwc", "n_binary")  

# Einfacher Q-Q Plot
data %>% 
  ggplot(aes(sample = e_liwc)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", x = "Theoretische Quantile", y = "Stichproben-Quantile")


# Violins
data %>% 
  violinJitter("o_liwc", "o_binary")  
data %>% 
  violinJitter("c_liwc", "c_binary")  
data %>% 
  violinJitter("e_liwc", "e_binary")  
data %>% 
  violinJitter("a_liwc", "a_binary")  
data %>% 
  violinJitter("n_liwc", "n_binary")  

# Boxplots
data %>% 
  boxplot("o_liwc", "o_binary")
data %>% 
  boxplot("c_liwc", "c_binary")
data %>% 
  boxplot("e_liwc", "e_binary")
data %>% 
  boxplot("a_liwc", "a_binary")
data %>% 
  boxplot("n_liwc", "n_binary")

# OCEAN gesamt
ocean <- liwc_data %>%
  select(o_liwc, c_liwc, e_liwc, a_liwc, n_liwc) %>%
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
data %>% 
  verteilung_multi(c("o_liwc", "c_liwc", "e_liwc", "a_liwc", "n_liwc"))

liwc_ausreisser <- data %>% filter(n_liwc < 3 && n_binary == "0")
liwc_ausreisser  

# Numerische Statistiken
data %>%
  group_by(o_binary) %>%
  summarise(
    mean = mean(o_liwc),
    sd = sd(o_liwc),
    n=n()
  )

data %>%
  group_by(c_binary) %>%
  summarise(
    mean = mean(c_liwc),
    sd = sd(c_liwc),
    n=n()
  )

data %>%
  group_by(e_binary) %>%
  summarise(
    mean = mean(e_liwc),
    sd = sd(e_liwc),
    n=n()
  )

data %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_liwc),
    sd = sd(a_liwc),
    n=n()
  )

data %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_liwc),
    sd = sd(n_liwc),
    n=n()
  )


