library("tidyverse")

essays <- tbl(con, "essays")
wang <- tbl(con, "wang_analyzation")
wang_joined <- left_join(essays, wang, by = c("id" = "essay_id"))
wang_truncated <- wang_joined %>% filter(classification_type == "truncated")
wang_sliding <- wang_joined %>% filter(classification_type == "slidingWindow")

source("sources/functions.R")




wang_sliding %>% 
#  filter(n_wang < 0.125) %>% 
  ggplot(aes(x = n_wang)) +
  geom_density() +
  geom_histogram() +
  labs(title = "Sliding Window",
       x = "Neurotizismus", y = "Values") +
  theme_minimal()



wang_joined %>% filter(c_binary == "1") %>% 
  select(c_wang) %>% 
  ggplot(aes(x = c_wang)) +
  geom_density() +
  theme_minimal()

# Gute Visualisierung nach binary!
wang_truncated %>% 
  filter(n_wang > 0.24) %>% 
  ggplot(aes(x = n_binary, y = n_wang, group = n_binary, fill=n_binary)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Sliding Window",
       x = "Neurotizismus", y = "Values") +
  theme_minimal()

# Gute Visualisierung nach binary!
wang_truncated %>% 
  #filter(a_wang > 0.24) %>% 
  ggplot(aes(x = a_binary, y = a_wang, group = a_binary, fill=a_binary)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Sliding Window",
       x = "Agreeableness", y = "Values") +
  theme_minimal()


wang_joined %>% 
#  filter(n_wang < 0.125) %>%
  filter(n_binary=="0") %>% 
  ggplot(aes(x = classification_type, y = n_wang, group = classification_type, fill=classification_type)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Truncate (N = 0)",
       x = "Methode", y = "Values") +
  theme_minimal()

wang_joined %>% 
  filter(n_wang < 0.125) %>%
  filter(n_binary=="1") %>% 
  ggplot(aes(x = classification_type, y = n_wang, group = classification_type, fill=classification_type)) +
  geom_violin() +
  geom_jitter(width=0.1,alpha=0.5)+
  labs(title = "Truncate (N = 1)",
       x = "Methode", y = "Values") +
  theme_minimal()


wang_sliding %>% 
  ggplot(aes(x = n_wang, group = n_binary, fill=n_binary)) +
  geom_density(alpha=0.5) +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()

wang_sliding %>% 
  ggplot(aes(x = o_wang, group = o_binary)) +
  geom_density() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()



wang_ausreisser <- wang_sliding %>% filter(n_wang > 0.13 && n_binary == 0)
wang_ausreisser  

wang_truncated %>%
  ggplot(aes(x = n_binary, y = n_wang, group = n_binary)) +
  geom_boxplot() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values") +
  theme_minimal()

wang_ausreisser <- wang_sliding %>% filter(n_wang < 0.115 && n_binary == 0)
wang_ausreisser  

wang_truncated %>% 
  ggplot(aes(x = n_wang, group = c(n_binary, classification_type))) +
  geom_density() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()

wang_truncated %>% 
  ggplot(aes(x = o_wang, group = o_binary)) +
  geom_density() +
  labs(title = "Method 1: Side-by-side Boxplots",
       x = "Groups", y = "Values", group="binary") +
  theme_minimal()





wang_truncated %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_wang),
    sd = sd(n_wang),
    n=n()
  )

wang_truncated %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_wang),
    sd = sd(a_wang),
    n=n()
  )

wang_sliding %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_wang),
    sd = sd(n_wang),
    n=n()
  )

# ANOVA der Binärgruppen nach Methode
# -> keine signifikanten Unterschiede
model_oneway <- aov(o_wang ~ o_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(o_wang ~ o_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(c_wang ~ c_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(c_wang ~ c_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(e_wang ~ e_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(e_wang ~ e_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(a_wang ~ a_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(a_wang ~ a_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(n_wang ~ n_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(n_wang ~ n_binary, data = wang_truncated)
summary(model_oneway)


# Faktorenanalyse
install.packages("lavaan")  # Install if not already installed
library(lavaan)             # Load the package

model <- '
  Ofactor =~ o_wang
  Cfactor =~ c_wang
  Efactor =~ e_wang
  Afactor =~ a_wang
  Nfactor =~ n_wang
'

fit <- cfa(model, data = tibble(wang_truncated))

summary(fit, fit.measures = TRUE, standardized = TRUE)
