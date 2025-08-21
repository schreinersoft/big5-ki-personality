library(tidyverse)
library(psych)
library(lavaan)
library(knitr)
library(semTools)
library(corrplot)
library(patchwork)
library(purrr)

source("connect_database.R")
source("functions.R")
source("BFI-2-Names.R")

# combine datasets
essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
openai <- tbl(con, "openai_analyzation") %>% 
  select(-updated_at) %>% 
  filter(essay_id <= 50) %>% 
  collect()
# join only rows with values
openai_joined <- left_join(essays, openai, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)

o_facets <- paste0("of", 1:3)
c_facets <- paste0("cf", 1:3)
e_facets <- paste0("ef", 1:3)
a_facets <- paste0("af", 1:3)
n_facets <- paste0("nf", 1:3)
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)

# Cronbachs alpha der Facetten
for (facets in facet_list) {
  alpha <- openai_joined %>% 
    select(all_of(facets)) %>% 
    as_tibble() %>% 
    alpha()
  print(alpha)
}

cor_matrix <- cor(openai_joined[, all_facets], use = "complete.obs")
# Round to 2 decimal places
cor_matrix_rounded <- round(cor_matrix, 2)
print(cor_matrix_rounded)
corrplot(cor_matrix_rounded, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8)
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Matrix")

# convert variables to binaries
openai_joined$o_bin <- ifelse(openai_joined$o_binary == "1", 1, 0)
openai_joined$c_bin <- ifelse(openai_joined$c_binary == "1", 1, 0)
openai_joined$e_bin <- ifelse(openai_joined$e_binary == "1", 1, 0)
openai_joined$a_bin <- ifelse(openai_joined$a_binary == "1", 1, 0)
openai_joined$n_bin <- ifelse(openai_joined$n_binary == "1", 1, 0)

# create aggregated results of llm calculations
llm_aggregations <- openai_joined %>%
  group_by(id) %>%
  summarise(
            o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
            c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
            e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
            a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
            n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
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

llm_analyzation_v1 <- left_join(llm_aggregations, essays, by = c("essay_id" = "id"))



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





# Verteilungen aggregierte LLM Ergebnisse
# Vergleiche nach Binärvariable
llm_analyzation_v1 %>% 
  verteilung("o_llm", "o_binary")  
llm_analyzation_v1 %>% 
  verteilung("c_llm", "c_binary")  
llm_analyzation_v1 %>% 
  verteilung("e_llm", "e_binary")  
llm_analyzation_v1 %>% 
  verteilung("a_llm", "a_binary")  
llm_analyzation_v1 %>% 
  verteilung("of1_agg", "n_binary")  

# Violins
llm_analyzation_v1 %>% 
  violinJitter("o_llm", "o_binary")  
llm_analyzation_v1 %>% 
  violinJitter("c_llm", "c_binary")  
llm_analyzation_v1 %>% 
  violinJitter("e_llm", "e_binary")  
llm_analyzation_v1 %>% 
  violinJitter("a_llm", "a_binary")  
llm_analyzation_v1 %>% 
  violinJitter("n_llm", "n_binary")  

# Boxplots
llm_analyzation_v1 %>% 
  boxplot("o_llm", "o_binary")
llm_analyzation_v1 %>% 
  boxplot("c_llm", "c_binary")
llm_analyzation_v1 %>% 
  boxplot("e_llm", "e_binary")
llm_analyzation_v1 %>% 
  boxplot("a_llm", "a_binary")
llm_analyzation_v1 %>% 
  boxplot("n_llm", "n_binary")


# Numerische Statistiken
llm_analyzation_v1 %>%
  group_by(o_binary) %>%
  summarise(
    mean = mean(o_llm),
    sd = sd(o_llm),
    n=n()
  )
llm_analyzation_v1 %>%
  group_by(c_binary) %>%
  summarise(
    mean = mean(c_llm),
    sd = sd(c_llm),
    n=n()
  )
llm_analyzation_v1 %>%
  group_by(e_binary) %>%
  summarise(
    mean = mean(e_llm),
    sd = sd(e_llm),
    n=n()
  )
llm_analyzation_v1 %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_llm),
    sd = sd(a_llm),
    n=n()
  )
llm_analyzation_v1 %>%
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

facets <- llm_analyzation_v1 %>% 
  select(of1, of2, of3, cf1, cf2, cf3, ef1, ef2, ef3, af1, af2, af3, nf1, nf2, nf3) %>% 
  drop_na() %>% 
  as_tibble()

fit <- cfa(model, data = facets, 
           estimator = "ML")
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

# PCA
pcModel <- principal(facets, nfactors = 5, rotate = "varimax")
pcModel
plot(pcModel$values, type = "b")


# ANOVA der Binärgruppen
model_oneway <- aov(o_llm ~ o_binary, data = llm_analyzation_v1)
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = llm_analyzation_v1)
summary(model_oneway)
model_oneway <- aov(e_llm ~ e_binary, data = llm_analyzation_v1)
summary(model_oneway)
model_oneway <- aov(a_llm ~ a_binary, data = llm_analyzation_v1)
summary(model_oneway)
model_oneway <- aov(n_llm ~ n_binary, data = llm_analyzation_v1)
summary(model_oneway)




