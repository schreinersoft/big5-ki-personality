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
source("NEO-PI-R-Names.R")

# combine datasets
essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
google_v4 <- tbl(con, "google_analyzation") %>% select(-updated_at) %>% 
  collect()
# join only rows with values
google_joined_v4 <- left_join(essays, google_v4, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)



google_v4 %>% 
  




google_joined_v4$temp.factor <- as.factor(google_joined_v4$temperature)

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)

# Korrekturen!  -> auch in model raus!
#o_facets <- o_facets[o_facets != "xxx"]

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]

facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)

temps <- sort(unique(google_joined_v4$temp.factor))

google_v4$temp.factor <- as.factor(google_v4$temperature)

google_v4 %>% 
ggplot(aes(x = of6, group=temp.factor, fill=temp.factor)) +
  geom_histogram(position="dodge",
                 breaks = seq(0.5, 9.5, by = 1),
                 bins = 9, 
                 boundary = 0.5,  # Ensures bins are centered on integers
                 color = "black") +
  labs(title = "hist",
       x = "Wert",
       y = "Anzahl") +
  scale_x_continuous(breaks = 1:9) +
  theme_minimal() 


# Cronbachs alpha der Facetten
for (facets in facet_list) {
  print(paste("Facet ----------------------------------------------------------"))
  for (temp in temps) {
    print(paste("Temperature: ",temp,"########################################################################"))
    alpha <- google_joined_v4 %>% 
      filter(temperature == temp) %>% 
      select(all_of(facets)) %>% 
      as_tibble() %>% 
      alpha()
    print(alpha)
  }
}

temp0 <- google_joined_v4 %>% filter(temperature == 0.0)
cor_matrix <- cor(temp0[, all_facets], use = "complete.obs")
# Round to 2 decimal places
cor_matrix_rounded <- round(cor_matrix, 2)
print(cor_matrix_rounded)
# with facet ids
corrplot(cor_matrix_rounded, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8)
# with facet names
rownames(cor_matrix_rounded) <- all_names
colnames(cor_matrix_rounded) <- all_names
corrplot(cor_matrix_rounded, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8)
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Matrix")

# convert variables to binaries
google_joined_v4$o_bin <- ifelse(google_joined_v4$o_binary == "1", 1, 0)
google_joined_v4$c_bin <- ifelse(google_joined_v4$c_binary == "1", 1, 0)
google_joined_v4$e_bin <- ifelse(google_joined_v4$e_binary == "1", 1, 0)
google_joined_v4$a_bin <- ifelse(google_joined_v4$a_binary == "1", 1, 0)
google_joined_v4$n_bin <- ifelse(google_joined_v4$n_binary == "1", 1, 0)

# create aggregated results of llm calculations
llm_aggregations <- google_joined_v4 %>%
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
            o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
            c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
            e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
            a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
            n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
            .groups = "drop") %>% 
  rename(essay_id = id)

###### with factor
# create aggregated results of llm calculations
llm_aggregations <- google_joined_v4 %>%
  group_by(id, temp.factor) %>%
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
            o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
            c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
            e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
            a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
            n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
            .groups = "drop") %>% 
  rename(essay_id = id)




llm_analyzation_v4_optimized <- left_join(llm_aggregations, essays, by = c("essay_id" = "id"))

# verteilungen nach temperatur
llm_analyzation_v4_optimized %>% 
  ggplot(aes(x = o_llm, fill=temp.factor)) +
  geom_density(alpha=0.5) +
  labs(title = "Density",
       x = "Value",
       y = "Density") +
  theme_minimal() 

llm_analyzation_v4_optimized %>% 
  ggplot(aes(x = c_llm, fill=temp.factor)) +
  geom_density(alpha=0.5) +
  labs(title = "Density",
       x = "Value",
       y = "Density") +
  theme_minimal() 

llm_analyzation_v4_optimized %>% 
  ggplot(aes(x = e_llm, fill=temp.factor)) +
  geom_density(alpha=0.5) +
  labs(title = "Density",
       x = "Value",
       y = "Density") +
  theme_minimal() 

llm_analyzation_v4_optimized %>% 
  ggplot(aes(x = a_llm, fill=temp.factor)) +
  geom_density(alpha=0.5) +
  labs(title = "Density",
       x = "Value",
       y = "Density") +
  theme_minimal() 

llm_analyzation_v4_optimized %>% 
  filter(o_binary == 0) %>% 
  ggplot(aes(x = n_llm, fill=temp.factor)) +
  geom_density(alpha=0.5) +
  labs(title = "Density",
       x = "Value",
       y = "Density") +
  theme_minimal() 

# Histogramme nach temperatur
llm_analyzation_v4_optimized %>% 
  ggplot(aes(x = ef2, fill=temp.factor)) +
  geom_histogram(position="dodge",
                 breaks = seq(0.5, 9.5, by = 1),
                 bins = 9, 
                 boundary = 0.5,  # Ensures bins are centered on integers
                 color = "black") +
  labs(title = "Histogramm",
       x = "Wert",
       y = "Anzahl") +
  scale_x_continuous(breaks = 1:9) +
  theme_minimal() 

# AUSGANGSWERTE
google_joined_v4 %>% 
  ggplot(aes(x = ef2, fill=temp.factor)) +
  geom_histogram(position="dodge",
                 breaks = seq(0.5, 9.5, by = 1),
                 bins = 9, 
                 boundary = 0.5,  # Ensures bins are centered on integers
                 color = "black") +
  labs(title = "Histogramm",
       x = "Wert",
       y = "Anzahl") +
  scale_x_continuous(breaks = 1:9) +
  theme_minimal() 

# Verteilungen Facetten
histogramm(google_joined_v4, "of1")
histogramm(google_joined_v4, "of2")
histogramm(google_joined_v4, "of3")
histogramm(google_joined_v4, "of4")
histogramm(google_joined_v4, "of5")
histogramm(google_joined_v4, "of6")

histogramm(google_joined_v4, "cf1")
histogramm(google_joined_v4, "cf2")
histogramm(google_joined_v4, "cf3")
histogramm(google_joined_v4, "cf4")
histogramm(google_joined_v4, "cf5")
histogramm(google_joined_v4, "cf6")

histogramm(google_joined_v4, "ef1")
histogramm(google_joined_v4, "ef2")
histogramm(google_joined_v4, "ef3")
histogramm(google_joined_v4, "ef4")
histogramm(google_joined_v4, "ef5")
histogramm(google_joined_v4, "ef6")

histogramm(google_joined_v4, "af1")
histogramm(google_joined_v4, "af2")
histogramm(google_joined_v4, "af3")
histogramm(google_joined_v4, "af4")
histogramm(google_joined_v4, "af5")
histogramm(google_joined_v4, "af6")

histogramm(google_joined_v4, "nf1")
histogramm(google_joined_v4, "nf2")
histogramm(google_joined_v4, "nf3")
histogramm(google_joined_v4, "nf1")
histogramm(google_joined_v4, "nf2")
histogramm(google_joined_v4, "nf3")

# Mehrfachhistogramme
google_joined_v4 %>% 
  histogramm_multi(o_facets)
google_joined_v4 %>% 
  histogramm_multi(c_facets)
google_joined_v4 %>% 
  histogramm_multi(e_facets)
google_joined_v4 %>% 
  histogramm_multi(a_facets)
google_joined_v4 %>% 
  histogramm_multi(n_facets)


google_joined_v4 %>% 
  histogramm_sechsfach(o_facets)
google_joined_v4 %>% 
  histogramm_sechsfach(c_facets)
google_joined_v4 %>% 
  filter(e_binary == 0) %>% 
  histogramm_sechsfach(e_facets)
google_joined_v4 %>% 
  filter(e_binary == 1) %>% 
  histogramm_sechsfach(e_facets)
google_joined_v4 %>% 
  histogramm_sechsfach(a_facets)
google_joined_v4 %>% 
  filter(n_binary == 0) %>% 
  histogramm_sechsfach(n_facets)
google_joined_v4 %>% 
  filter(n_binary == 1) %>% 
  histogramm_sechsfach(n_facets)


# Verteilungen aggregierte LLM Ergebnisse
# Vergleiche nach Binärvariable
llm_analyzation_v4_optimized %>% 
  verteilung("o_llm", "o_binary")  
llm_analyzation_v4_optimized %>% 
  verteilung("c_llm", "c_binary")  
llm_analyzation_v4_optimized %>% 
  verteilung("e_llm", "e_binary")  
llm_analyzation_v4_optimized %>% 
  verteilung("a_llm", "a_binary")  
llm_analyzation_v4_optimized %>% 
  verteilung("n_llm", "n_binary")  

# Violins
llm_analyzation_v4_optimized %>% 
  violinJitter("o_llm", "o_binary")  
llm_analyzation_v4_optimized %>% 
  violinJitter("c_llm", "c_binary")  
llm_analyzation_v4_optimized %>% 
  violinJitter("e_llm", "e_binary")  
llm_analyzation_v4_optimized %>% 
  violinJitter("a_llm", "a_binary")  
llm_analyzation_v4_optimized %>% 
  violinJitter("n_llm", "n_binary")  

# Boxplots
llm_analyzation_v4_optimized %>% 
  boxplot("o_llm", "o_binary")
llm_analyzation_v4_optimized %>% 
  boxplot("c_llm", "c_binary")
llm_analyzation_v4_optimized %>% 
  boxplot("e_llm", "e_binary")
llm_analyzation_v4_optimized %>% 
  boxplot("a_llm", "a_binary")
llm_analyzation_v4_optimized %>% 
  boxplot("n_llm", "n_binary")


# Numerische Statistiken
llm_analyzation_v4_optimized %>%
  group_by(o_binary) %>%
  summarise(
    mean = mean(o_llm),
    sd = sd(o_llm),
    n=n()
  )
llm_analyzation_v4_optimized %>%
  group_by(c_binary) %>%
  summarise(
    mean = mean(c_llm),
    sd = sd(c_llm),
    n=n()
  )
llm_analyzation_v4_optimized %>%
  group_by(e_binary) %>%
  summarise(
    mean = mean(e_llm),
    sd = sd(e_llm),
    n=n()
  )
llm_analyzation_v4_optimized %>%
  group_by(a_binary) %>%
  summarise(
    mean = mean(a_llm),
    sd = sd(a_llm),
    n=n()
  )
llm_analyzation_v4_optimized %>%
  group_by(n_binary) %>%
  summarise(
    mean = mean(n_llm),
    sd = sd(n_llm),
    n=n()
  )


# Konfirmatorische Faktorenanalyse
model <- '
  Ofactor =~ of1 + of2 + of3 + of4 + of5 + of6
  Cfactor =~ cf1 + cf2 + cf3 + cf4 + cf5 + cf6
  Efactor =~ ef1 + ef2 + ef3 + ef4 + ef5 + ef6
  Afactor =~ af1 + af2 + af3 + af4 + af5 + af6
  Nfactor =~ nf1 + nf2 + nf3 + nf4 + nf5 + nf6
'

facets <- llm_analyzation_v4_optimized %>% 
  select(all_of(all_facets)) %>% 
  drop_na() %>% 
  as_tibble()


# CFA erst ab ca. 200 punkten sinnvoll
#fit <- cfa(model, data = facets, 
#           estimator = "GLS")
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
model_oneway <- aov(o_llm ~ o_binary, data = llm_analyzation_v4_optimized %>% filter(temp.factor == 0.0))
summary(model_oneway)
model_oneway <- aov(o_llm ~ o_binary, data = llm_analyzation_v4_optimized %>% filter(temp.factor == 0.2))
summary(model_oneway)
model_oneway <- aov(o_llm ~ o_binary, data = llm_analyzation_v4_optimized %>% filter(temp.factor == 0.4))
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = llm_analyzation_v4_optimized %>% filter(temp.factor == 0.0))
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = llm_analyzation_v4_optimized %>% filter(temp.factor == 0.2))
summary(model_oneway)
model_oneway <- aov(c_llm ~ c_binary, data = llm_analyzation_v4_optimized %>% filter(temp.factor == 0.4))
summary(model_oneway)
model_oneway <- aov(e_llm ~ e_binary, data = llm_analyzation_v4_optimized)
summary(model_oneway)
model_oneway <- aov(a_llm ~ a_binary, data = llm_analyzation_v4_optimized)
summary(model_oneway)
model_oneway <- aov(n_llm ~ n_binary, data = llm_analyzation_v4_optimized)
summary(model_oneway)



