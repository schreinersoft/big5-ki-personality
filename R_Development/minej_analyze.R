library("tidyverse")

essays <- tbl(con, "essays")
minej <- tbl(con, "minej_analyzation")
minej_joined <- left_join(essays, minej, by = c("id" = "essay_id"))
minej_truncated <- minej_joined %>% filter(classification_type == "truncated")
minej_sliding <- minej_joined %>% filter(classification_type == "slidingWindow")

source("functions.R")

# ANOVA der Binärgruppen
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



# Vergleiche nach Binärvariable
# Verteilungen
minej_joined %>% 
  verteilung("o_minej", "o_binary")  
minej_joined %>% 
  verteilung("c_minej", "c_binary")  
minej_joined %>% 
  verteilung("e_minej", "e_binary")  
minej_joined %>% 
  verteilung("a_minej", "a_binary")  
minej_joined %>% 
  verteilung("n_minej", "n_binary")  

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


