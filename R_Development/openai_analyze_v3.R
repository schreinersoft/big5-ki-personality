library(tidyverse)
library(psych)
library(lavaan)
library(knitr)
library(semTools)
library(corrplot)

source("connect_database.R")
source("functions.R")

# combine datasets
essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
openai_v3 <- tbl(con, "openai_analyzation_v3") %>% select(-updated_at) %>% collect
# join only rows with values
openai_joined_v3 <- left_join(essays, openai_v3, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)

# Cronbachs alpha der Facetten
for (facets in facet_list) {
alpha <- openai_joined_v3 %>% 
  select(all_of(facets)) %>% 
  as_tibble() %>% 
  alpha()
print(alpha)
}

cor_matrix <- cor(openai_joined_v3[, all_facets], use = "complete.obs")
# Round to 2 decimal places
cor_matrix_rounded <- round(cor_matrix, 2)
print(cor_matrix_rounded)
corrplot(cor_matrix_rounded, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8)
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Matrix")

# convert variables to binaries
openai_joined_v3$o_bin <- ifelse(openai_joined_v3$o_binary == "1", 1, 0)
openai_joined_v3$c_bin <- ifelse(openai_joined_v3$c_binary == "1", 1, 0)
openai_joined_v3$e_bin <- ifelse(openai_joined_v3$e_binary == "1", 1, 0)
openai_joined_v3$a_bin <- ifelse(openai_joined_v3$a_binary == "1", 1, 0)
openai_joined_v3$n_bin <- ifelse(openai_joined_v3$n_binary == "1", 1, 0)

# create aggregated results of llm calculations
llm_aggregations <- openai_joined_v3 %>%
  group_by(id) %>%
  summarise(of1 = mean(of1, na.rm = TRUE),
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
            of4 = mean(of4, na.rm = TRUE),
            cf4 = mean(cf4, na.rm = TRUE),
            ef4 = mean(ef4, na.rm = TRUE),
            af4 = mean(af4, na.rm = TRUE),
            nf4 = mean(nf4, na.rm = TRUE),
            of5 = mean(of5, na.rm = TRUE),
            cf5 = mean(cf5, na.rm = TRUE),
            ef5 = mean(ef5, na.rm = TRUE),
            af5 = mean(af5, na.rm = TRUE),
            nf5 = mean(nf5, na.rm = TRUE),
            of6 = mean(of6, na.rm = TRUE),
            cf6 = mean(cf6, na.rm = TRUE),
            ef6 = mean(ef6, na.rm = TRUE),
            af6 = mean(af6, na.rm = TRUE),
            nf6 = mean(nf6, na.rm = TRUE),
            o_llm = mean(of1, of2, of3, of4, of5, of6, na.rm = TRUE),
            c_llm = mean(cf1, cf2, cf3, cf4, cf5, cf6, na.rm = TRUE),
            e_llm = mean(ef1, ef2, ef3, ef4, ef5, ef6, na.rm = TRUE),
            a_llm = mean(af1, af2, af3, af4, af5, af6, na.rm = TRUE),
            n_llm = mean(nf1, nf2, nf3, nf4, nf5, nf6, na.rm = TRUE),
            .groups = "drop") %>% 
  rename(essay_id = id)

llm_analyzation_v3 <- left_join(llm_aggregations, essays, by = c("essay_id" = "id"))



# Verteilungen Facetten
histogramm(openai_joined_v3, "of1")
histogramm(openai_joined_v3, "of2")
histogramm(openai_joined_v3, "of3")
histogramm(openai_joined_v3, "of4")
histogramm(openai_joined_v3, "of5")
histogramm(openai_joined_v3, "of6")

histogramm(openai_joined_v3, "cf1")
histogramm(openai_joined_v3, "cf2")
histogramm(openai_joined_v3, "cf3")
histogramm(openai_joined_v3, "cf1")
histogramm(openai_joined_v3, "cf2")
histogramm(openai_joined_v3, "cf3")

histogramm(openai_joined_v3, "ef1")
histogramm(openai_joined_v3, "ef2")
histogramm(openai_joined_v3, "ef3")
histogramm(openai_joined_v3, "ef1")
histogramm(openai_joined_v3, "ef2")
histogramm(openai_joined_v3, "ef3")

histogramm(openai_joined_v3, "af1")
histogramm(openai_joined_v3, "af2")
histogramm(openai_joined_v3, "af3")
histogramm(openai_joined_v3, "af1")
histogramm(openai_joined_v3, "af2")
histogramm(openai_joined_v3, "af3")

histogramm(openai_joined_v3, "nf1")
histogramm(openai_joined_v3, "nf2")
histogramm(openai_joined_v3, "nf3")
histogramm(openai_joined_v3, "nf1")
histogramm(openai_joined_v3, "nf2")
histogramm(openai_joined_v3, "nf3")

openai_joined_v3 %>% 
  histogramm_multi(o_facets)
openai_joined_v3 %>% 
  histogramm_multi(c_facets)
openai_joined_v3 %>% 
  histogramm_multi(e_facets)
openai_joined_v3 %>% 
  histogramm_multi(a_facets)
openai_joined_v3 %>% 
  histogramm_multi(n_facets)


violinJitter(openai_joined_v3, "nf3","n_binary")
boxplot(openai_joined_v3, "nf3", "n_binary")

# Verteilungskurven
ocean <- openai_joined_v3 %>%
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

openai_joined_v3 %>% 
  verteilung_multi(c("o_openai", "c_openai", "e_openai", "a_openai", "n_openai"))

# Verteilungen aggregierte LLM Ergebnisse
# Vergleiche nach Binärvariable
llm_analyzation_v3 %>% 
  verteilung("o_llm", "o_binary")  
llm_analyzation_v3 %>% 
  verteilung("c_llm", "c_binary")  
llm_analyzation_v3 %>% 
  verteilung("e_llm", "e_binary")  
llm_analyzation_v3 %>% 
  verteilung("a_llm", "a_binary")  
llm_analyzation_v3 %>% 
  verteilung("n_llm", "n_binary")  

# Violins
llm_analyzation_v3 %>% 
  violinJitter("o_llm", "o_binary")  
llm_analyzation_v3 %>% 
  violinJitter("c_llm", "c_binary")  
llm_analyzation_v3 %>% 
  violinJitter("e_llm", "e_binary")  
llm_analyzation_v3 %>% 
  violinJitter("a_llm", "a_binary")  
llm_analyzation_v3 %>% 
  violinJitter("n_llm", "n_binary")  

# Boxplots
llm_analyzation_v3 %>% 
  boxplot("o_llm", "o_binary")
llm_analyzation_v3 %>% 
  boxplot("c_llm", "c_binary")
llm_analyzation_v3 %>% 
  boxplot("e_llm", "e_binary")
llm_analyzation_v3 %>% 
  boxplot("a_llm", "a_binary")
llm_analyzation_v3 %>% 
  boxplot("n_llm", "n_binary")


# Numerische Statistiken
llm_analyzation_v3 %>%
  group_by(o_binary) %>%
  summarise(
    mean = mean(o_llm),
    sd = sd(o_llm),
    n=n()
  )
llm_analyzation_v3 %>%
  group_by(c_binary) %>%
  summarise(
    mean = mean(c_llm),
    sd = sd(c_llm),
    n=n()
  )
llm_analyzation_v3 %>%
  group_by(e_binary) %>%
  summarise(
    mean = mean(e_llm),
    sd = sd(e_llm),
    n=n()
  )
llm_analyzation_v3 %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_llm),
    sd = sd(a_llm),
    n=n()
  )
llm_analyzation_v3 %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_llm),
    sd = sd(n_llm),
    n=n()
  )


# Faktorenanalyse
model <- '
  Ofactor =~ of1 + of2 + of3 + of4 + of5 + of6
  Cfactor =~ cf1 + cf2 + cf3 + cf4 + cf5 + cf6
  Efactor =~ ef1 + ef2 + ef3 + ef4 + ef5 + ef6
  Afactor =~ af1 + af2 + af3 + af4 + af5 + af6
  Nfactor =~ nf1 + nf2 + nf3 + nf4 + nf5 + nf6
'

facets <- llm_analyzation_v3 %>% 
  select(all_of(all_facets)) %>% 
  drop_na() %>% 
  as_tibble()

fit <- cfa(model, data = facets, 
           estimator = "MLR")
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
model_oneway <- aov(o_llm ~ o_binary, data = llm_analyzation_v3)
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = llm_analyzation_v3)
summary(model_oneway)
model_oneway <- aov(e_llm ~ e_binary, data = llm_analyzation_v3)
summary(model_oneway)
model_oneway <- aov(a_llm ~ a_binary, data = llm_analyzation_v3)
summary(model_oneway)
model_oneway <- aov(n_llm ~ n_binary, data = llm_analyzation_v3)
summary(model_oneway)




