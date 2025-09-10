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
liwc_model <- liwc %>% 
  filter(essay_id <= 250) %>% 
  select(-id)

db_write_model(liwc_model, "liwc")

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

# OCEAN type
liwc_data <- liwc_data %>% 
  mutate(
    ocean_type = sum(
      o_bin * 1,
      c_bin * 2,
      e_bin * 4,
      a_bin * 8,
      n_bin * 16
    )
  )






######################################### DEPRECATED
### create a similarity score
liwc_data <- liwc_data %>% 
  rowwise() %>% 
  mutate(
    o_liwc_z_prod = o_liwc_z * o_bin_z,
    c_liwc_z_prod = c_liwc_z * c_bin_z,
    e_liwc_z_prod = e_liwc_z * e_bin_z,
    a_liwc_z_prod = a_liwc_z * a_bin_z,
    n_liwc_z_prod = n_liwc_z * n_bin_z,
    liwc_sim_score = sum(across(ends_with("z_prod")))
  )

# DIES wäre der Kennwert
mean(liwc_data$liwc_sim_score)



liwc_data %>% 
  group_by(sametendency) %>% 
  summarize(
    n=n(),
    mean=mean(liwc_sim_score)
  )
liwc_data %>% 
  ggplot(aes(x=liwc_sim_score, group=sametendency, fill=sametendency))+
  geom_density()

numEssays <- function(threshold){
  liwc_data %>% 
    filter(liwc_sim_score >= threshold) %>% 
    nrow()
}

tibble(
  threshold = seq(-5, 10, 0.1)
) %>% 
  rowwise() %>% 
  mutate(essays = numEssays(threshold)) %>% 
  ggplot(aes(x=threshold, y=essays)) + 
  geom_col()


# verteilung der OCEAN Typen

liwc_data %>% 
  group_by(ocean_type) %>% 
  summarise(
    n = n(),
    mean = mean(liwc_sim_score)
  ) %>% 
  ggplot(aes(x=ocean_type, y=n)) +
  geom_col() +
  theme_minimal()



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


liwc_data %>%
  filter(id ==86) %>% 
  select(all_of(ends_with("_z")))







# find out 
threshold <- 0.5

liwc_fit <- liwc_data %>% 
  filter(
    (abs(o_liwc_z) > threshold) &
    (abs(c_liwc_z) > threshold) &
    (abs(e_liwc_z) > threshold) &
    (abs(a_liwc_z) > threshold) &
    (abs(n_liwc_z) > threshold)) %>% 
  filter(sametendency ==5) # %>% 
liwc_fit
#  group_by(sametendency) %>% 
#  summarize(n = n()) %>% 
#  select(n) %>% 
#  as.numeric()


# calculate euclidian distances
eucl_dist <- function (x,y) {
  sqrt((x-y)^2)
}

liwc_data <- liwc_data %>% 
  mutate(o_dist = eucl_dist(o_bin_z, o_liwc_z),
         c_dist = eucl_dist(c_bin_z, o_liwc_z),
         e_dist = eucl_dist(e_bin_z, o_liwc_z),
         a_dist = eucl_dist(a_bin_z, o_liwc_z),
         n_dist = eucl_dist(n_bin_z, o_liwc_z),
         dist = rowSums(across(ends_with("_dist"))))

describe(liwc_data$dist)

liwc_data %>% 
  group_by(sametendency) %>% 
  count()

# Density over all sum of distances
liwc_data %>% 
  ggplot(aes(x = dist)) +
  xlim(0, max(liwc_data$dist)) +
  geom_density(color = "black",
               fill = "lightyellow") +
  labs(title = "Ähnlichkeit der Bewertungen",
       x = "Euklidische Distanz",
       y = "Dichte") +
  theme_minimal() 

# Number of essays by threshold
threshold <- seq(0, 15, 0.2)

count_essays <- function(threshold){
  liwc_data %>% 
    filter(dist <= threshold) %>% 
    count()
}

numEssays <- tibble(threshold = threshold) %>% 
  rowwise() %>% 
  mutate(essays = sum(count_essays(threshold)))

describe(numEssays)

numEssays %>% 
  ggplot(aes(x = threshold, y = essays)) +
  geom_col(color= "black", fill="lightyellow") + 
  labs(title = "Essays nach Abstandsmass",
       x = "Schwellwert",
       y= "Anzahl") +
  theme_minimal()

numEssays %>% 
  filter(same) %>% 
  tibble(threshold = threshold) %>% 
  rowwise() %>% 
  mutate(essays = sum(count_essays(threshold)))

