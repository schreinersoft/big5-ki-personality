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


# Hole nur ein Essay mit 150 Messungen
google_v4 <- tbl(con, "google_analyzation") %>% select(-updated_at) %>% 
  filter(model=="gemini-2.5-flash-lite", essay_id==14) %>% 
  collect()
google_v4$temp.factor <- as.factor(google_v4$temperature)

o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)


google_v4 %>% 
  filter(temperature==0.4) %>% 
  histogramm_sechsfach(o_facets)
google_v4 %>% 
  filter(temperature==1.0) %>% 
  histogramm_sechsfach(o_facets)

google_v4 %>% 
  filter(temperature==0.4) %>% 
  histogramm_sechsfach(c_facets)
google_v4 %>% 
  filter(temperature==1.0) %>% 
  histogramm_sechsfach(c_facets)
google_v4 %>% 
  filter(temperature==0.4) %>% 
  histogramm_sechsfach(e_facets)
google_v4 %>% 
  filter(temperature==1.0) %>% 
  histogramm_sechsfach(e_facets)
google_v4 %>% 
  filter(temperature==0.4) %>% 
  histogramm_sechsfach(a_facets)
google_v4 %>% 
  filter(temperature==1.0) %>% 
  histogramm_sechsfach(a_facets)
google_v4 %>% 
  filter(temperature==0.4) %>% 
  histogramm_sechsfach(n_facets)
google_v4 %>% 
  filter(temperature==1.0) %>% 
  histogramm_sechsfach(n_facets)

# create aggregated results
llm_aggregations <- google_v4 %>%
  group_by(temp.factor) %>%
  summarise(
            #of1sd = sd(of1, na.rm = TRUE),
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
            .groups = "drop")

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


# Mehrfachhistogramme
google_v4 %>% 
  histogramm_multi(o_facets)
google_joined_v4 %>% 
  histogramm_multi(c_facets)
google_joined_v4 %>% 
  histogramm_multi(e_facets)
google_joined_v4 %>% 
  histogramm_multi(a_facets)
google_joined_v4 %>% 
  histogramm_multi(n_facets)

model_oneway <- aov(o_facets ~ temp.factor, data = google_v4)
summary(model_oneway)

