library(tidyverse)
library(psych)
library(lavaan)
library(knitr)
library(semTools)

source("connect_database.R")
source("functions.R")

# combine datasets
essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
openai <- tbl(con, "openai_analyzation") %>% select(-updated_at) %>% collect
# join only rows with values
openai_joined <- left_join(essays, openai, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)

# Cronbachs alpha der Facetten
Oalpha <- openai_joined %>% 
  select(of1, of2, of3) %>% 
  as_tibble() %>% 
  alpha()
print(Oalpha)
Calpha <- openai_joined %>% 
  select(cf1, cf2, cf3) %>% 
  as_tibble() %>% 
  alpha()
print(Calpha)
Ealpha <- openai_joined %>% 
  select(ef1, ef2, ef3) %>% 
  as_tibble() %>% 
  alpha()
print(Ealpha)
Aalpha <- openai_joined %>% 
  select(af1, af2, af3) %>% 
  as_tibble() %>% 
  alpha()
print(Aalpha)
Nalpha <- openai_joined %>% 
  select(nf1, nf2, nf3) %>% 
  as_tibble() %>% 
  alpha()
print(Nalpha)


# convert variables to binaries
openai_joined$o_bin <- ifelse(openai_joined$o_binary == "1", 1, 0)
openai_joined$c_bin <- ifelse(openai_joined$c_binary == "1", 1, 0)
openai_joined$e_bin <- ifelse(openai_joined$e_binary == "1", 1, 0)
openai_joined$a_bin <- ifelse(openai_joined$a_binary == "1", 1, 0)
openai_joined$n_bin <- ifelse(openai_joined$n_binary == "1", 1, 0)

# create aggregated results of llm calculations
llm_aggregations <- openai_joined %>%
  group_by(id) %>%
  summarise(o_llm = mean(c(of1, of2, of3), na.rm = TRUE),
            c_llm = mean(c(cf1, cf2, cf3), na.rm = TRUE),
            e_llm = mean(c(ef1, ef2, ef3), na.rm = TRUE),
            a_llm = mean(c(af1, af2, af3), na.rm = TRUE),
            n_llm = mean(c(nf1, nf2, nf3), na.rm = TRUE),
            of1 = mean(of1, na.rm = TRUE),
            cf1 = mean(cf1, na.rm = TRUE),
            ef1 = mean(ef1, na.rm = TRUE),
            af1 = mean(af1, na.rm = TRUE),
            nf1 = mean(nf1, na.rm = TRUE),
            of2 = mean(of2, na.rm = TRUE),
            cf2 = mean(cf2, na.rm = TRUE),
            ef2 = mean(ef2, na.rm = TRUE),
            af2 = mean(af2, na.rm = TRUE),
            nf2 = mean(nf2, na.rm = TRUE),
            of3 = mean(of3, na.rm = TRUE),
            cf3 = mean(cf3, na.rm = TRUE),
            ef3 = mean(ef3, na.rm = TRUE),
            af3 = mean(af3, na.rm = TRUE),
            nf3 = mean(nf3, na.rm = TRUE),
            .groups = "drop") %>% 
            rename(essay_id = id)

data <- left_join(llm_aggregations, essays, by = c("essay_id" = "id"))



# Verteilungen Facetten
histogramm(openai_joined, "of1")
histogramm(openai_joined, "of2")
histogramm(openai_joined, "of3")

histogramm(openai_joined, "cf1")
histogramm(openai_joined, "cf2")
histogramm(openai_joined, "cf3")

histogramm(openai_joined, "ef1")
histogramm(openai_joined, "ef2")
histogramm(openai_joined, "ef3")

histogramm(openai_joined, "af1")
histogramm(openai_joined, "af2")
histogramm(openai_joined, "af3")

histogramm(openai_joined, "nf1")
histogramm(openai_joined, "nf2")
histogramm(openai_joined, "nf3")

openai_joined %>% 
  histogramm_multi(c("of1", "of2", "of3"))
openai_joined %>% 
  histogramm_multi(c("cf1", "cf2", "cf3"))
openai_joined %>% 
  histogramm_multi(c("ef1", "ef2", "ef3"))
openai_joined %>% 
  histogramm_multi(c("af1", "af2", "af3"))
openai_joined %>% 
  histogramm_multi(c("nf1", "nf2", "nf3"))



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

# Verteilungen aggregierte LLM Ergebnisse
# Vergleiche nach Binärvariable
data %>% 
  verteilung("o_llm", "o_binary")  
data %>% 
  verteilung("c_llm", "c_binary")  
data %>% 
  verteilung("e_llm", "e_binary")  
data %>% 
  verteilung("a_llm", "a_binary")  
data %>% 
  verteilung("of1_agg", "n_binary")  

# Violins
data %>% 
  violinJitter("o_llm", "o_binary")  
data %>% 
  violinJitter("c_llm", "c_binary")  
data %>% 
  violinJitter("e_llm", "e_binary")  
data %>% 
  violinJitter("a_llm", "a_binary")  
data %>% 
  violinJitter("n_llm", "n_binary")  

# Boxplots
data %>% 
  boxplot("o_llm", "o_binary")
data %>% 
  boxplot("c_llm", "c_binary")
data %>% 
  boxplot("e_llm", "e_binary")
data %>% 
  boxplot("a_llm", "a_binary")
data %>% 
  boxplot("n_llm", "n_binary")


# Numerische Statistiken
data %>%
  group_by(o_binary) %>%
  summarise(
    mean = mean(o_llm),
    sd = sd(o_llm),
    n=n()
  )
data %>%
  group_by(c_binary) %>%
  summarise(
    mean = mean(c_llm),
    sd = sd(c_llm),
    n=n()
  )
data %>%
  group_by(e_binary) %>%
  summarise(
    mean = mean(e_llm),
    sd = sd(e_llm),
    n=n()
  )
data %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_llm),
    sd = sd(a_llm),
    n=n()
  )
data %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_llm),
    sd = sd(n_llm),
    n=n()
  )


# Faktorenanalyse
model <- '
  Ofactor =~ of1 + of2 + of3
  Cfactor =~ cf1 + cf2 + cf3
  Efactor =~ ef1 + ef2 + ef3
  Afactor =~ af1 + af2 + af3
  Nfactor =~ nf1 + nf2 + nf3
'

facets <- data %>% 
  select(of1, of2, of3, cf1, cf2, cf3, ef1, ef2, ef3, af1, af2, af3, nf1, nf2, nf3) %>% 
  drop_na() %>% 
  as_tibble()

fit <- cfa(model, data = facets, 
           estimator = "GLS")
           #,
           #se="bootstrap",
           #bootstrap = 2000) # see CFA.md

summary(fit, fit.measures = TRUE, standardized = TRUE)
parameterEstimates(fit)
standardizedSolution(fit)  # -1... +1
reliability(fit) 
inspect(fit, "cor.lv")
modificationIndices(fit, sort = TRUE)
residuals(fit, type = "standardized")

# ANOVA der Binärgruppen
model_oneway <- aov(o_llm ~ o_binary, data = data)
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = data)
summary(model_oneway)
model_oneway <- aov(e_llm ~ e_binary, data = data)
summary(model_oneway)
model_oneway <- aov(a_llm ~ a_binary, data = data)
summary(model_oneway)
model_oneway <- aov(n_llm ~ n_binary, data = data)
summary(model_oneway)




