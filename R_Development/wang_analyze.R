library("tidyverse")

essays <- tbl(con, "essays")
wang <- tbl(con, "wang_analyzation")
wang_joined <- left_join(essays, wang, by = c("id" = "essay_id"))
wang_truncated <- wang_joined %>% filter(classification_type == "truncated")
wang_sliding <- wang_joined %>% filter(classification_type == "slidingWindow")

source("sources/functions.R")

# ANOVA der Binärgruppen
# -> truncated etwas besser, warum auch immer???
model_oneway <- aov(o_wang ~ o_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(o_wang ~ o_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(c_wang ~ c_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(c_wang ~ c_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(e_wang ~ e_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(e_wang ~ e_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(a_wang ~ a_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(a_wang ~ a_binary, data = wang_sliding)
summary(model_oneway)
model_oneway <- aov(n_wang ~ n_binary, data = wang_truncated)
summary(model_oneway)
model_oneway <- aov(n_wang ~ n_binary, data = wang_sliding)
summary(model_oneway)



# Vergleiche nach Binärvariable
# Verteilungen
wang_joined %>% 
  verteilung("o_wang", "o_binary")  
wang_joined %>% 
  verteilung("c_wang", "c_binary")  
wang_joined %>% 
  verteilung("e_wang", "e_binary")  
wang_joined %>% 
  verteilung("a_wang", "a_binary")  
wang_joined %>% 
  verteilung("n_wang", "n_binary")  

# Violins
wang_joined %>% 
  violinJitter("o_wang", "o_binary")  
wang_joined %>% 
  violinJitter("c_wang", "c_binary")  
wang_joined %>% 
  violinJitter("e_wang", "e_binary")  
wang_joined %>% 
  violinJitter("a_wang", "a_binary")  
wang_joined %>% 
  violinJitter("n_wang", "n_binary")  

# Boxplots
wang_joined %>% 
  boxplot("o_wang", "o_binary")
wang_joined %>% 
  boxplot("c_wang", "c_binary")
wang_joined %>% 
  boxplot("e_wang", "e_binary")
wang_joined %>% 
  boxplot("a_wang", "a_binary")
wang_joined %>% 
  boxplot("n_wang", "n_binary")

# Histogramme
wang_joined %>% 
  histogramm("o_wang", "o_binary")
wang_joined %>% 
  histogramm("c_wang", "c_binary")
wang_joined %>% 
  histogramm("e_wang", "e_binary")
wang_joined %>% 
  histogramm("a_wang", "a_binary")
wang_joined %>% 
  histogramm("n_wang", "n_binary")


# OCEAN gesamt
ocean <- wang_joined %>%
  select(o_wang, c_wang, e_wang, a_wang, n_wang) %>%
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
wang_joined %>% 
  verteilung_multi(c("o_wang", "c_wang", "e_wang", "a_wang", "n_wang"))

wang_ausreisser <- wang_joined %>% filter(n_wang < 3 && n_binary == "0")
wang_ausreisser  

# Numerische Statistiken
wang_joined %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_wang),
    sd = sd(n_wang),
    n=n()
  )

wang_joined %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_wang),
    sd = sd(a_wang),
    n=n()
  )

wang_joined %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_wang),
    sd = sd(n_wang),
    n=n()
  )

