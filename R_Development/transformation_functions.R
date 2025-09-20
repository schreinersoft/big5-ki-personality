library("tidyverse")


# calculate all means of factors by hash
aggregate_model_by_hash <- function(d)
{
  o_facets <- d %>% select(starts_with(("of"))) %>% names()
  c_facets <- d %>% select(starts_with(("cf"))) %>% names()
  e_facets <- d %>% select(starts_with(("ef"))) %>% names()
  a_facets <- d %>% select(starts_with(("af"))) %>% names()
  n_facets <- d %>% select(starts_with(("nf"))) %>% names()
  
  all_facet_names <- c(o_facets,c_facets, e_facets, 
                       a_facets, n_facets)
  
  # Aggregate the data to obtain 1 measurement per hash
  da <- d %>%
    select(hash, all_of(all_facet_names)) %>% 
    group_by(hash) %>%
    summarise(
      across(all_of(all_facet_names), ~ mean(.x, na.rm = TRUE)),
      o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
      c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
      e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
      a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
      n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
      .groups = "drop") %>% 
    select(hash, all_of(ends_with("_llm")))
  
  return(da)
}



# calculate all means of facets and factors
aggregate_model <- function(d)
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

# calculate all means of facets and factors
aggregate_model_hash <- function(d)
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
    select(hash, all_of(all_facet_names)) %>% 
    group_by(hash) %>%
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


get_variable_name <- function(var) {
  deparse(substitute(var))
}

format_p_psych <- function(p_value) {
  ifelse(p_value < 0.001, "<.001", format_psych(sprintf("%.3f", as.numeric(p_value))))
}

format_psych <- function(value) {
  # Entferne führende "0" vor dem Dezimalkomma
  gsub("^(-?)0\\.", ".", value)
}


# needs dataframe with essay_id bins and o_x as input
create_scores_frame <- function(d)
{
  all_factors <- d %>% select(
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")) %>% names()

  all_bins <- d %>% 
    select(starts_with("bin_")) %>% 
    names()
  
  d_z <- d %>% 
    select(essay_id, all_of(all_bins),all_of(all_factors)) %>% 
    mutate(across(all_of(all_bins), as.numeric)) %>% 
    mutate(across(all_of(all_bins), scale)) %>% 
    mutate(across(all_of(all_factors), scale)) %>% 
    mutate(across(all_of(all_factors), as.numeric)) %>% 
    rowwise() %>% 
    mutate(
      rowmax = max(abs(c_across(all_of(all_factors)))),
      NormO = (get(all_factors[1])/rowmax),
      NormC = (get(all_factors[2])/rowmax),
      NormE = (get(all_factors[3])/rowmax),
      NormA = (get(all_factors[4])/rowmax),
      NormN = (get(all_factors[5])/rowmax),
      SO = (1 - sqrt((get(all_bins[1]) - NormO)^2))*100,
      SC = (1 - sqrt((get(all_bins[2]) - NormC)^2))*100,
      SE = (1 - sqrt((get(all_bins[3]) - NormE)^2))*100,
      SA = (1 - sqrt((get(all_bins[4]) - NormA)^2))*100,
      SN = (1 - sqrt((get(all_bins[5]) - NormN)^2))*100,
      SCORE = (SO + SC + SE + SA + SN)/5
    )
  return (d_z)
}
