# create aggregated results of llm calculations for 3 factors
data_aggregated <- data %>%
  group_by(essay_id) %>%
  summarise(
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
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
    .groups = "drop")

data_facets <- data_aggregated %>% 
  select(all_of(all_facets)) %>% 
  as_tibble()

get_variable_name <- function(var) {
  deparse(substitute(var))
}

OLDmodel_aggregate <- function(d)
{
  o_facets <- d %>% select(starts_with(("of")))
  c_facets <- d %>% select(starts_with(("cf")))
  e_facets <- d %>% select(starts_with(("ef")))
  a_facets <- d %>% select(starts_with(("af")))
  n_facets <- d %>% select(starts_with(("nf")))
  all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
  facet_list <- c()
  for (facet in all_facets) { facet_list <- c(facet_list, c(get_variable_name(facet))) }
  
  da <- d %>%
    group_by(essay_id) %>%
    summarise(
      across(all_of(all_facets), ~ mean(.x, na.rm = TRUE)),
      o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
      c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
      e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
      a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
      n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
      .groups = "drop")
}


model_aggregate <- function(d)
{
  o_facets <- d %>% select(starts_with(("of")))
  c_facets <- d %>% select(starts_with(("cf")))
  e_facets <- d %>% select(starts_with(("ef")))
  a_facets <- d %>% select(starts_with(("af")))
  n_facets <- d %>% select(starts_with(("nf")))
  
  all_facet_names <- c(names(o_facets), names(c_facets), names(e_facets), 
                       names(a_facets), names(n_facets))
  
  
  
  # Aggregate the data
  da <- d %>%
    select(essay_id, all_of(all_facet_names)) %>% 
    group_by(essay_id) %>%
    summarise(
      across(all_of(all_facet_names), ~ mean(.x, na.rm = TRUE)),
      o_llm = mean(c_across(all_of(names(o_facets))), na.rm = TRUE),
      c_llm = mean(c_across(all_of(names(c_facets))), na.rm = TRUE),
      e_llm = mean(c_across(all_of(names(e_facets))), na.rm = TRUE),
      a_llm = mean(c_across(all_of(names(a_facets))), na.rm = TRUE),
      n_llm = mean(c_across(all_of(names(n_facets))), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(da)
}



