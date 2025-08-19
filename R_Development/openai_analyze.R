library(tidyverse)
library(psych)
library(lavaan)

source("functions.R")

# combine datasets
essays <- tbl(con, "essays")
openai <- tbl(con, "openai_analyzation") %>% filter(essay_id > 9) # XXX only for first analyzation!
openai_joined <- left_join(essays, openai, by = c("id" = "essay_id"))


verteilung(openai_joined, "o_openai", "o_binary")
verteilung(openai_joined, "of1")
verteilung(openai_joined, "of2")
verteilung(openai_joined, "of3")
histogramm(openai_joined, "o_openai", "o_binary")
histogramm(openai_joined, "of1")
histogramm(openai_joined, "of2")
histogramm(openai_joined, "of3")

openai_joined %>% 
  histogramm_multi(c("of1", "of2", "of3"))

openai_joined %>% 
  filter(o_binary == "1") %>% 
  histogramm("o_openai")

verteilung(openai_joined, "c_openai")
verteilung(openai_joined, "cf1")
verteilung(openai_joined, "cf2")
verteilung(openai_joined, "cf3")

verteilung(openai_joined, "e_openai")
verteilung(openai_joined, "ef1")
verteilung(openai_joined, "ef2")
verteilung(openai_joined, "ef3")

verteilung(openai_joined, "a_openai")
verteilung(openai_joined, "af1")
verteilung(openai_joined, "af2")
verteilung(openai_joined, "af3")

verteilung(openai_joined, "n_openai")
verteilung(openai_joined, "nf1")
verteilung(openai_joined, "nf2")
verteilung(openai_joined, "nf3")
violinJitter(openai_joined, "nf3","n_binary")


boxplot(openai_joined, "nf3", "n_binary")


# Verteilungskurven
ocean <- openai_joined %>%
  select(o_openai, c_openai, e_openai, a_openai, n_openai) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")
# OCEAN gesamt
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

openai_joined %>% 
  verteilung_multi(c("o_openai", "c_openai", "e_openai", "a_openai", "n_openai"))


# Vergleiche nach Binärvariable
openai_joined %>% 
  verteilung("o_openai", "o_binary")  
openai_joined %>% 
  verteilung("c_openai", "c_binary")  
openai_joined %>% 
  verteilung("e_openai", "e_binary")  
openai_joined %>% 
  verteilung("a_openai", "a_binary")  
openai_joined %>% 
  verteilung("n_openai", "n_binary")  

# Violins
openai_joined %>% 
  violinJitter("o_openai", "o_binary")  
openai_joined %>% 
  violinJitter("c_openai", "c_binary")  
openai_joined %>% 
  violinJitter("e_openai", "e_binary")  
openai_joined %>% 
  violinJitter("a_openai", "a_binary")  
openai_joined %>% 
  violinJitter("n_openai", "n_binary")  

# Boxplots
openai_joined %>% 
  boxplot_grouped("o_openai", "o_binary")
openai_joined %>% 
  boxplot_grouped("c_openai", "c_binary")
openai_joined %>% 
  boxplot_grouped("e_openai", "e_binary")
openai_joined %>% 
  boxplot_grouped("a_openai", "a_binary")
openai_joined %>% 
  boxplot_grouped("n_openai", "n_binary")


openai_ausreisser <- openai_joined %>% filter(n_openai < 3 && n_binary == "0")
openai_ausreisser  

# Numerische Statistiken
openai_joined %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_openai),
    sd = sd(n_openai),
    n=n()
  )

openai_joined %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_openai),
    sd = sd(a_openai),
    n=n()
  )

openai_joined %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_openai),
    sd = sd(n_openai),
    n=n()
  )

# ANOVA der Binärgruppen
model_oneway <- aov(o_openai ~ o_binary, data = openai_joined)
summary(model_oneway)
model_oneway <- aov(c_openai ~ c_binary, data = openai_joined)
summary(model_oneway)
model_oneway <- aov(e_openai ~ e_binary, data = openai_joined)
summary(model_oneway)
model_oneway <- aov(a_openai ~ a_binary, data = openai_joined)
summary(model_oneway)
model_oneway <- aov(n_openai ~ n_binary, data = openai_joined)
summary(model_oneway)

# Cronbachs alpha
Oalpha <- openai_joined %>% 
  select(of1, of2, of3) %>% 
  as_tibble() %>% 
  alpha()
summary(Oalpha)
Calpha <- openai_joined %>% 
  select(cf1, cf2, cf3) %>% 
  as_tibble() %>% 
  alpha()
summary(Calpha)
Ealpha <- openai_joined %>% 
  select(ef1, ef2, ef3) %>% 
  as_tibble() %>% 
  alpha()
summary(Ealpha)
Aalpha <- openai_joined %>% 
  select(af1, af2, af3) %>% 
  as_tibble() %>% 
  alpha()
summary(Aalpha)
Nalpha <- openai_joined %>% 
  select(nf1, nf2, nf3) %>% 
  as_tibble() %>% 
  alpha()
summary(Nalpha)


# Faktorenanalyse
install.packages("lavaan")  # Install if not already installed
library(lavaan)             # Load the package

model <- '
  Ofactor =~ of1 + of2 + of3
  Cfactor =~ cf1 + cf2 + cf3
  Efactor =~ ef1 + ef2 + ef3
  Afactor =~ af1 + af2 + af3
  Nfactor =~ nf1 + nf2 + nf3
'

facets <- openai_joined %>% 
  select(of1, of2, of3, cf1, cf2, cf3, ef1, ef2, ef3, af1, af2, af3, nf1, nf2, nf3) %>% 
  na.omit() %>% 
  as_tibble()

fit <- cfa(model, data = facets)

summary(fit, fit.measures = TRUE, standardized = TRUE)


