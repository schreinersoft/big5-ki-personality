library("tidyverse")

essays <- tbl(con, "essays")
minej <- tbl(con, "minej_analyzation")
minej_joined <- left_join(essays, minej, by = c("id" = "essay_id"))
minej_truncated <- minej_joined %>% filter(classification_type == "truncated")
minej_sliding <- minej_joined %>% filter(classification_type == "slidingWindow")


minej_joined %>% 
  select(n_minej) %>% 
  pull()


minej_sliding %>% 
#  filter(n_minej < 0.125) %>% 
  ggplot(aes(x = n_minej)) +
  geom_density() +
  geom_histogram() +
  labs(title = "Sliding Window",
       x = "Neurotizismus", y = "Values") +
  theme_minimal()



minej_joined %>% filter(c_binary == "1") %>% 
  select(c_minej) %>% 
  ggplot(aes(x = c_minej)) +
  geom_density() +
  theme_minimal()

# Gute Visualisierung nach binary!
minej_truncated %>% 
  ggplot(aes(x = n_binary, y = n_minej, group = n_binary, fill=n_binary)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Sliding Window",
       x = "Neurotizismus", y = "Values") +
  theme_minimal()

# Gute Visualisierung nach binary!
minej_sliding %>% 
  ggplot(aes(x = a_binary, y = a_minej, group = a_binary, fill=a_binary)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Sliding Window",
       x = "Agreeableness", y = "Values") +
  theme_minimal()


minej_joined %>% 
  filter(n_binary=="0") %>% 
  ggplot(aes(x = classification_type, y = n_minej, group = classification_type, fill=classification_type)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Truncate (N = 0)",
       x = "Methode", y = "Values") +
  theme_minimal()

minej_joined %>% 
  filter(n_minej < 0.125) %>%
  filter(n_binary=="1") %>% 
  ggplot(aes(x = classification_type, y = n_minej, group = classification_type, fill=classification_type)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Truncate (N = 1)",
       x = "Methode", y = "Values") +
  theme_minimal()


minej_truncated %>% 
  ggplot(aes(x = e_minej, group = e_binary, fill=e_binary)) +
  geom_density(alpha=0.5) +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()

minej_sliding %>% 
  ggplot(aes(x = o_minej, group = o_binary)) +
  geom_density() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()



minej_ausreisser <- minej_sliding %>% filter(n_minej > 0.13 && n_binary == 0)
minej_ausreisser  

minej_truncated %>%
  ggplot(aes(x = n_binary, y = n_minej, group = n_binary)) +
  geom_boxplot() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values") +
  theme_minimal()

minej_ausreisser <- minej_sliding %>% filter(n_minej < 0.115 && n_binary == 0)
minej_ausreisser  

minej_truncated %>% 
  ggplot(aes(x = n_minej, group = c(n_binary, classification_type))) +
  geom_density() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()

minej_truncated %>% 
  ggplot(aes(x = o_minej, group = o_binary)) +
  geom_density() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()





minej_truncated %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_minej),
    sd = sd(n_minej),
    n=n()
  )

minej_truncated %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_minej),
    sd = sd(a_minej),
    n=n()
  )

minej_sliding %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_minej),
    sd = sd(n_minej),
    n=n()
  )

# ANOVA der Binärgruppen nach Methode
# -> keine signifikanten Unterschiede
model_oneway <- aov(o_minej ~ o_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(o_minej ~ o_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(c_minej ~ c_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(c_minej ~ c_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(e_minej ~ e_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(e_minej ~ e_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(a_minej ~ a_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(a_minej ~ a_binary, data = minej_truncated)
summary(model_oneway)
model_oneway <- aov(n_minej ~ n_binary, data = minej_sliding)
summary(model_oneway)
model_oneway <- aov(n_minej ~ n_binary, data = minej_truncated)
summary(model_oneway)


# Faktorenanalyse
install.packages("lavaan")  # Install if not already installed
library(lavaan)             # Load the package

model <- '
  Ofactor =~ o_minej
  Cfactor =~ c_minej
  Efactor =~ e_minej
  Afactor =~ a_minej
  Nfactor =~ n_minej
'

fit <- cfa(model, data = tibble(minej_truncated))

summary(fit, fit.measures = TRUE, standardized = TRUE)
