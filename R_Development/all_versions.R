### Collector for all models, data and analyzations

source("connect_database.R")
source("graphics_functions.R")
source("tables_functions.R")
source("transformation_functions.R")
source("connect_database.R")

root_folder <- "C:/Users/bernd/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen"
tables_output_folder <- paste(root_folder, "/tables", sep="")
graphics_output_folder <- paste(root_folder, "/graphics", sep="")
stats_output_folder <- paste(root_folder, "/outputs", sep="")

fetch_raw_data <- function(table_name)
{
  data_raw <- tbl(con, table_name) %>% 
    select(-id, -updated_at, -input_tokens, -output_tokens) %>% 
    collect() %>% 
    select(where(~ !any(is.na(.))))
  return (data_raw)
}

analyze_all <- function(data, model_version)
{
  analyze_alpha_omega(data, model_version)
  analyze_factor_loadings(data, model_version)
  analyze_item_statistics(data, model_version)
}

create_all_graphics <- function(data, model_version)
{
  create_correlation_matrices(data, model_version)
  create_facet_densities(data, model_version)
  create_factor_densities(data, model_version)
}





################################################# V1.0
model_version <- "v1.0"
data <- tbl(con, "openai_analyzation") %>% 
  select(-temperature) %>% 
  collect() %>% 
  rename(
    of1b = of1,
    of2b = of2,
    of3b = of3,
    cf1b = cf1,
    cf2b = cf2,
    cf3b = cf3,
    ef1b = ef1,
    ef2b = ef2,
    ef3b = ef3,
    af1b = af1,
    af2b = af2,
    af3b = af3,
    nf1b = nf1,
    nf2b = nf2,
    nf3b = nf3
  )
data_aggregated <- aggregate_model(data)
#db_write_model(data_aggregated, model_version)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)
analyze_all(data_aggregated, model_version)
create_all_graphics(data_aggregated, model_version)


################################################# V1.1
model_version <- "v1.1"
data_aggregated <- data %>% 
  select(-ef2b) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)

################################################# V1.2
model_version <- "v1.2"
data_aggregated <- data %>% 
  select(-ef2b, -af2b) %>% 
  aggregate_model()

db_write_model(data_aggregated, model_version)

analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)
analyze_item_statistics(data_aggregated, model_version)

create_all_graphics(data_aggregated, model_version)



################################################# V2.0
model_version <- "v2.0"



################################################# V5.0
model_version <- "v5.0"
o_facets <- c("of3b", "of1", "of2", "of5")
c_facets <- c("cf2b", "cf3b", "cf3", "cf5")
e_facets <- c("ef2", "ef3b", "ef4", "ef5")
a_facets <- c("af1b", "af1", "af3", "af6")
n_facets <- c("nf1", "nf4", "nf6")
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
create_correlation_matrices(data_aggregated, model_version)
create_facet_densities(data_aggregated, model_version)
create_factor_densities(data_aggregated, model_version)
analyze_alpha_omega(data_aggregated, model_version)
analyze_factor_loadings(data_aggregated, model_version)


################################################# V5.1
model_version <- "v5.1"
data_raw_v5 <- fetch_raw_data("openai_analyzation_v5")

data <- data_raw_v5 %>% 
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  filter(essay_id <=250)
data_aggregated <- aggregate_model(data)

create_essay_histograms(data, model_version, 27)
create_essay_histograms(data, model_version, 42)
create_essay_histograms(data, model_version, 112)
create_all(data_aggregated, model_version)

