library(tidyverse)
library(rstatix)
library(psych)
library(flextable)

source("connect_database.R")
source("functions.R")
source("Factor-Names-EN.R")

essays <- tbl(con, "essays")  %>% select(-text, -author) %>% collect()
liwc <- tbl(con, "liwc_analyzation")  %>% select(-updated_at, -liwc_all) %>% collect
liwc_data <- left_join(essays, liwc, by = c("id" = "essay_id")) %>% 
  drop_na(o_liwc)

# z-Normalisierung
liwc_data$o_liwc_z <- as.numeric(scale(liwc_data$o_liwc))
liwc_data$c_liwc_z <- as.numeric(scale(liwc_data$c_liwc))
liwc_data$e_liwc_z <- as.numeric(scale(liwc_data$e_liwc))
liwc_data$a_liwc_z <- as.numeric(scale(liwc_data$a_liwc))
liwc_data$n_liwc_z <- as.numeric(scale(liwc_data$n_liwc))

liwc_data$o_bin <- ifelse(liwc_data$o_binary == "1", 1, 0)
liwc_data$c_bin <- ifelse(liwc_data$c_binary == "1", 1, 0)
liwc_data$e_bin <- ifelse(liwc_data$e_binary == "1", 1, 0)
liwc_data$a_bin <- ifelse(liwc_data$a_binary == "1", 1, 0)
liwc_data$n_bin <- ifelse(liwc_data$n_binary == "1", 1, 0)
liwc_data$o_bin_z <- ifelse(liwc_data$o_binary == "1", 1, -1)
liwc_data$c_bin_z <- ifelse(liwc_data$c_binary == "1", 1, -1)
liwc_data$e_bin_z <- ifelse(liwc_data$e_binary == "1", 1, -1)
liwc_data$a_bin_z <- ifelse(liwc_data$a_binary == "1", 1, -1)
liwc_data$n_bin_z <- ifelse(liwc_data$n_binary == "1", 1, -1)

# OCEAN type (2^5 = 32 Typen)
liwc_data <- liwc_data %>% 
  rowwise() %>% 
  mutate(
    ocean_type = sum(c(
      o_bin * 1,
      c_bin * 2,
      e_bin * 4,
      a_bin * 8,
      n_bin * 16)
    )
  )

# Korrelationen der 5 Faktoren mit LIWC
cor.test(liwc_data$o_bin, liwc_data$o_liwc)
cor.test(liwc_data$o_bin, liwc_data$o_liwc, method = "spearman")


















# Tests auf Normalverteilung
# --> alle normalverteilt!
vec <- liwc_data %>%
  select(o_liwc) %>% 
  pull(o_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(c_liwc) %>% 
  pull(c_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(e_liwc) %>% 
  pull(e_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(a_liwc) %>% 
  pull(a_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

vec <- liwc_data %>%
  select(n_liwc) %>% 
  pull(n_liwc)
ks.test(vec, "pnorm", mean(vec), sd(vec))

# Vergleiche nach Binärvariable
# Verteilungen
liwc_data %>% 
  z_verteilung_title("o_liwc_z", "O", 3)  
liwc_data %>% 
  z_verteilung_title("c_liwc_z", "C", 3)  
liwc_data %>% 
  z_verteilung_title("e_liwc_z", "E", 3)  
liwc_data %>% 
  z_verteilung_title("a_liwc_z", "A", 3)  
liwc_data %>% 
  z_verteilung_title("n_liwc_z", "N", 3)  

# analyze OCEAN factors of all liwc data
plots <- list()
plots[[1]] <- liwc_data %>% 
    z_verteilung_title("o_liwc_z", factor_names[1], 3)  
plots[[2]] <- liwc_data %>% 
  z_verteilung_title("c_liwc_z", factor_names[2], 3)  
plots[[3]] <- liwc_data %>% 
  z_verteilung_title("e_liwc_z", factor_names[3], 3)  
plots[[4]] <- liwc_data %>% 
  z_verteilung_title("a_liwc_z", factor_names[4], 3)  
plots[[5]] <- liwc_data %>% 
  z_verteilung_title("n_liwc_z", factor_names[5], 3)  

combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plot_layout(ncol = 3)
combined_plot
ggsave("graphics/density_liwc_factors.png", plot = combined_plot, dpi=300, width = 8, height = 6)


# find out which liwc and binary codings tend to same direction
liwc_data$o_sametendency <- ifelse(liwc_data$o_liwc_z * liwc_data$o_bin_z > 0, 1, 0)
liwc_data$c_sametendency <- ifelse(liwc_data$c_liwc_z * liwc_data$c_bin_z > 0, 1, 0)
liwc_data$e_sametendency <- ifelse(liwc_data$e_liwc_z * liwc_data$e_bin_z > 0, 1, 0)
liwc_data$a_sametendency <- ifelse(liwc_data$a_liwc_z * liwc_data$a_bin_z > 0, 1, 0)
liwc_data$n_sametendency <- ifelse(liwc_data$n_liwc_z * liwc_data$n_bin_z > 0, 1, 0)

liwc_data <- liwc_data %>% 
  mutate(sametendency = rowSums(across(ends_with("_sametendency"))))


