# Model version for printing
model_version <- "v5.0"

source("connect_database.R")
source("functions.R")
source("Factor-Names-EN.R")

o_facets <- c("of3b", "of1", "of2", "of5")
c_facets <- c("cf2b", "cf3b", "cf3", "cf5")
e_facets <- c("ef2", "ef3b", "ef4", "ef5")
a_facets <- c("af1b", "af1", "af3", "af6")
n_facets <- c("nf1", "nf4", "nf6")

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_names <- list("Creative Imagination", "Fantasy", "Aesthetics", "Ideas",
                  "Productiveness", "Responsibility", "Dutifulness", "Self-Discipline",
                  "Gregariousness", "Energy Level", "Activity", "Excitement-seeking", 
                  "Compassion", "Trust", "Altruism", "Tender-mindedness", 
                  "Anxiety", "Self-consciousness", "Vulnerability")

all_factors <- c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
all_factor_names <- factor_names[all_factors]

# combine V1 and V2 aggregated
data_bfi <- tbl(con, "openai_analyzation") %>% 
  select(essay_id, of3, cf2, cf3, ef3, af1) %>% 
  collect() %>% 
  group_by(essay_id) %>%
  summarise(
    of3 = mean(of3, na.rm = TRUE),
    cf2 = mean(cf2, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    ef3 = mean(ef3, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    .groups = "drop") %>% 
  rename(of3b = of3,
         cf2b = cf2,
         cf3b = cf3,
         ef3b = ef3,
         af1b = af1,
         essay_idb = essay_id)

# simulation of V5 !
# combine datasets               DANGER !!! v2 in publication is v3 XXX
data_neo <- tbl(con, "openai_analyzation_v3") %>%
  select(essay_id, of1, of2, of5, cf3, cf5, ef2, ef4, ef5, af1, af3, af6, nf1, nf4, nf6) %>% 
  collect() %>% 
  group_by(essay_id) %>%
  summarise(
    of1 = mean(of1, na.rm = TRUE),
    of2 = mean(of2, na.rm = TRUE),
    of5 = mean(of5, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    cf5 = mean(cf5, na.rm = TRUE),
    ef2 = mean(ef2, na.rm = TRUE),
    ef4 = mean(ef4, na.rm = TRUE),
    ef5 = mean(ef5, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    af3 = mean(af3, na.rm = TRUE),
    af6 = mean(af6, na.rm = TRUE),
    nf1 = mean(nf1, na.rm = TRUE),
    nf4 = mean(nf4, na.rm = TRUE),
    nf6 = mean(nf6, na.rm = TRUE),
    .groups = "drop")


data_aggregated <- left_join(data_bfi, data_neo, by = c("essay_idb" = "essay_id")) %>%
  rowwise() %>% 
  mutate(
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE)
  )
  
  

data_facets <- data_aggregated %>% 
  select(all_of(all_facets))
data_factors <- data_aggregated %>% 
  select(all_of(all_factors))

sink(paste("outputs/omega_analyzation_", model_version, ".txt"))
source("omega.R")
sink()

sink(paste("outputs/output_analyzation_", model_version, ".txt"))

source("macros_v5.R")

sink()
