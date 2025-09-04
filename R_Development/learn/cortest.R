
## TESt
cortest <- tibble(
  c(1,2,3,4,5),
  c(-1,1,1,-1,1)
)
cor(cortest)

cortest <- tibble(
  c(-1,2,3,-8,5),
  c(-1,1,1,-1,1)
)
cor(cortest)

cortest <- tibble(
  c(-1,2,3,-8,5),
  c(-1,1,1,-1,1)
)
cor(cortest)

tibble(
  c(-1,1),
  c(1,-1)
) %>% cor() %>% tibble()

tibble(
  c(1,0),
  c(1,-1)
) %>% cor()
  

corr <- tibble(
  c(7,3,6,8,2),
  c(1,1,-1,1,1)
) %>% cor()



bin_vars <- liwc_data %>% 
  select(o_bin, c_bin, e_bin, a_bin, n_bin)
liwc_vars <- liwc_data %>% 
  select(o_liwc, c_liwc, e_liwc, a_liwc, n_liwc)
o_bins <- liwc_data %>% 
  select(o_bin)
o_liwcs <- liwc_data %>% 
  select(o_liwc)

matrix <- cor(bin_vars, liwc_vars)
paired <- diag(matrix)
paired

cor(c(1,2,3), c(1,2,2))


cors <-
c(
  cor(liwc_data$o_bin, liwc_data$o_liwc),
  cor(liwc_data$c_bin, liwc_data$c_liwc),
  cor(liwc_data$e_bin, liwc_data$e_liwc),
  cor(liwc_data$a_bin, liwc_data$a_liwc),
  cor(liwc_data$n_bin, liwc_data$n_liwc)
)
cors_spearman <-
  c(
    cor(liwc_data$o_bin, liwc_data$o_liwc, method = "spearman"),
    cor(liwc_data$c_bin, liwc_data$c_liwc, method = "spearman"),
    cor(liwc_data$e_bin, liwc_data$e_liwc, method = "spearman"),
    cor(liwc_data$a_bin, liwc_data$a_liwc, method = "spearman"),
    cor(liwc_data$n_bin, liwc_data$n_liwc, method = "spearman")
  )

cor.test(liwc_data$o_bin, liwc_data$o_liwc)

cor.test(liwc_data$o_bin, liwc_data$o_liwc, method = "spearman")



liwc_data <- liwc_data %>% 
  rowwise() %>% 
  mutate(
    corr = cor(c(o_bin, c_bin, e_bin, a_bin, n_bin),
               c(o_liwc, c_liwc, e_liwc, a_liwc, n_liwc))
  )

max(abs(c(-11,4)))

2^4


liwc_data %>% 
  ggplot(aes(x=corr)) +
  geom_density()

liwc_data %>% 
  select(corr) %>% 
  describe()


analyze <- liwc_data %>% 
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
    So = 1 - sqrt((o_bin_z - po)^2),
    Sc = 1 - sqrt((c_bin_z - pc)^2),
    Se = 1 - sqrt((e_bin_z - pe)^2),
    Sa = 1 - sqrt((a_bin_z - pa)^2),
    Sn = 1 - sqrt((n_bin_z - pn)^2),
    SCORE = (So + Sc + Se + Sa + Sn)/5
  )

analyze %>% 
  select(po,pc,pe,pa,pn, SCORE) %>% 
  describe()


analyze %>% 
  ggplot +
  geom_density(aes(x=po)) +
  geom_density(aes(x=pc)) +
  geom_density(aes(x=pe)) +
  geom_density(aes(x=pa)) +
  geom_density(aes(x=pn)) + 
  geom_density(aes(x=SCORE, color="red"))

# Alle Einzelscores
analyze %>% 
  select(So, Sc, Se, Sa, Sn) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, color = "darkgrey", group = variable, fill=variable)) +
  geom_density(alpha=0.7)+
  scale_fill_brewer(type = "qual", palette = "Oranges")

# Alle Pfs
analyze %>% 
  select(po, pc, pe, pa, pn) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, color = "darkgrey", fill = variable)) +
  geom_density(alpha=0.7)+
  scale_fill_brewer(type = "qual", palette = "Oranges")




  
analyze %>% 
  ggplot +
  geom_density(aes(x=SCORE), fill="yellow")
  


vec <- analyze$SCORE
ks.test(vec, "pnorm", mean(vec), sd(vec))

# Density over all sum of distances
analyze %>% 
  ggplot(aes(x = SCORE)) +
  xlim(-1, 1) +
  geom_density(color = "black",
               fill = "lightyellow") +
  labs(title = "Ähnlichkeit der Bewertungen",
       x = "SCORE",
       y = "Dichte") +
  theme_minimal()   


# Number of essays by threshold
count_essays <- function(thr)
{
  analyze %>% 
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
  geom_col(color= "black", fill="lightyellow") + 
  labs(title = "Essays nach Abstandsmass",
       x = "Schwellwert",
       y= "Anzahl") +
  theme_minimal()



# empirisch: Schwelle bei ca. 0.6
thr <- 0.6

# insgesamt?
analyze %>% 
  select(essay_id, SCORE) %>% 
  filter(SCORE>0.6)

# oder alle gemeinsam?
analyze %>% 
  filter(
    (So >= thr) &
    (Sc >= thr) &
    (Se >= thr) &
    (Sa >= thr) &
    (Sn >= thr))
      

         