# Model version for printing
modelVersion <- "v5.1"

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


# combine V1 and V2 aggregated
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(essay_id, all_of(all_facets), model) %>% 
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  collect()

# create aggregated results of llm calculations for 6 facets and Temperature
data_aggregated <- data %>%
  group_by(essay_id) %>%
  summarise(
    of3b = mean(of3b, na.rm = TRUE),
    of1 = mean(of1, na.rm = TRUE),
    of2 = mean(of2, na.rm = TRUE),
    of5 = mean(of5, na.rm = TRUE),
    cf2b = mean(cf2b, na.rm = TRUE),
    cf3b = mean(cf3b, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    cf5 = mean(cf5, na.rm = TRUE),
    ef2 = mean(ef2, na.rm = TRUE),
    ef3b = mean(ef3b, na.rm = TRUE),
    ef4 = mean(ef4, na.rm = TRUE),
    ef5 = mean(ef5, na.rm = TRUE),
    af1b = mean(af1b, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    af3 = mean(af3, na.rm = TRUE),
    af6 = mean(af6, na.rm = TRUE),
    nf1 = mean(nf1, na.rm = TRUE),
    nf4 = mean(nf4, na.rm = TRUE),
    nf6 = mean(nf6, na.rm = TRUE),
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(
    o_llm_z = as.numeric(scale(o_llm)),
    c_llm_z = as.numeric(scale(c_llm)),
    e_llm_z = as.numeric(scale(e_llm)),
    a_llm_z = as.numeric(scale(a_llm)),
    n_llm_z = as.numeric(scale(n_llm))
  )


data_facets <- data_aggregated %>% 
  select(all_of(all_facets)) %>% 
  as_tibble()

sink(paste("outputs/output_analyzation_", modelVersion, ".txt"))

source("macros_v5.R")

sink()

data_aggregated %>%
  filter(essay_id ==27) 
