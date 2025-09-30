library(tidyverse)
library(rstatix)
library(psych)
library(flextable)

source("sources/connect_database.R")
source("sources/functions.R")
source("sources/Factor-Names-EN.R")

essays <- tbl(con, "essays")  %>% select(-author) %>% collect()
liwc <- tbl(con, "liwc_analyzation")  %>% select(-updated_at, -liwc_all) %>% collect
liwc_data <- left_join(essays, liwc, by = c("id" = "essay_id")) %>% 
  drop_na(o_liwc)

liwc_ocean <- c("o_liwc_z", "c_liwc_z", "e_liwc_z", "a_liwc_z", "n_liwc_z")


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


####################### Korrelationen der 5 Faktoren mit LIWC
cor.test(liwc_data$o_bin, liwc_data$o_liwc)



####################### Bilde Scores
score_analyze <- liwc_data %>% 
  select(id, o_bin_z, c_bin_z, e_bin_z, a_bin_z, n_bin_z, o_liwc_z, c_liwc_z, e_liwc_z, a_liwc_z, n_liwc_z) %>% 
  rename(essay_id = id) %>% 
  rowwise() %>% 
  mutate(
    rowmax = max(abs(c(o_liwc_z, c_liwc_z, e_liwc_z, a_liwc_z, n_liwc_z))),
    No = (o_liwc_z/rowmax),
    Nc = (c_liwc_z/rowmax),
    Ne = (e_liwc_z/rowmax),
    Na = (a_liwc_z/rowmax),
    Nn = (n_liwc_z/rowmax),
    So = 1 - sqrt((o_bin_z - No)^2),
    Sc = 1 - sqrt((c_bin_z - Nc)^2),
    Se = 1 - sqrt((e_bin_z - Ne)^2),
    Sa = 1 - sqrt((a_bin_z - Na)^2),
    Sn = 1 - sqrt((n_bin_z - Nn)^2),
    SCORE = (So + Sc + Se + Sa + Sn)/5
  )

score_analyze %>% 
  select(No,Nc,Ne,Na,Nn,So,Sc,Se,Sa,Sn, SCORE) %>% 
  describe()


score_analyze %>% 
  ggplot +
  geom_density(aes(x=No)) +
  geom_density(aes(x=Nc)) +
  geom_density(aes(x=Ne)) +
  geom_density(aes(x=Na)) +
  geom_density(aes(x=Nn)) + 
  geom_density(aes(x=SCORE, color="red")) +
  theme_minimal()


# Alle Sfs
score_analyze %>% 
  select(So, Sc, Se, Sa, Sn) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, color = "darkgrey", group = variable, fill=variable)) +
  geom_density(alpha=0.7)+
  scale_fill_brewer(type = "qual", palette = "Oranges")

# Alle Nfs
score_analyze %>% 
  select(No, Nc, Ne, Na, Nn) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, color = "darkgrey", fill = variable)) +
  geom_density(alpha=0.7)+
  scale_fill_brewer(type = "qual", palette = "Oranges")


score_analyze %>% 
  ggplot +
  geom_density(aes(x=SCORE), fill="yellow")


# Number of essays by threshold
count_essays <- function(thr)
{
  score_analyze %>% 
    filter(SCORE >= thr) %>% 
    count() %>% sum()
}

threshold <- seq(-1, 1, 0.01)
essays_filtered <- tibble(threshold = threshold) %>% 
  rowwise() %>% 
  mutate(
    essays = sum(count_essays(threshold)))
describe(essays_filtered)
essays_filtered %>% 
  ggplot(aes(x = threshold, y = essays)) +
  geom_col(color= "black", fill="orange") + 
  labs(title = "Essays nach Abstandsmass",
       x = "Schwellwert",
       y= "Anzahl") +
  theme_minimal()






# empirisch: Schwelle bei ca. 0.6
thr <- 0.45

# insgesamt?
score_analyze %>% 
  select(essay_id, SCORE, all_of(liwc_ocean)) %>% 
  filter(SCORE >= thr) %>% 
  arrange(desc(SCORE))


# as.array()# oder alle gemeinsam?
score_analyze %>% 
  filter(
    (So >= thr) &
      (Sc >= thr) &
      (Se >= thr) &
      (Sa >= thr) &
      (Sn >= thr))

enhanced_essays <- score_analyze %>% 
  select(essay_id, SCORE) %>% 
  filter(SCORE >= thr)

mean(enhanced_essays$SCORE)

best <- score_analyze %>% 
  arrange(desc(SCORE)) %>% 
  select(essay_id, So, Sc, Se, Sa, Sn, SCORE)

# Write data
dbWriteTable(con, "liwc_best", best, 
             overwrite = TRUE, row.names = FALSE)

best %>% filter(essay_id <= 250)


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


