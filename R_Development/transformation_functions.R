library("tidyverse")

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

get_variable_name <- function(var) {
  deparse(substitute(var))
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
    mutate(across(all_of(all_bins), scale)) %>% 
    mutate(across(all_of(all_bins), as.numeric)) %>% 
    mutate(across(all_of(all_factors), scale)) %>% 
    mutate(across(all_of(all_factors), as.numeric)) %>% 
    rowwise() %>% 
    mutate(
      rowmax = max(abs(c_across(all_of(all_factors)))),
      No = (get(all_factors[1])/rowmax),
      Nc = (get(all_factors[2])/rowmax),
      Ne = (get(all_factors[3])/rowmax),
      Na = (get(all_factors[4])/rowmax),
      Nn = (get(all_factors[5])/rowmax),
      So = 1 - sqrt((get(all_bin[1]) - No)^2),
      Sc = 1 - sqrt((get(all_bin[2]) - Nc)^2),
      Se = 1 - sqrt((get(all_bin[3]) - Ne)^2),
      Sa = 1 - sqrt((get(all_bin[4]) - Na)^2),
      Sn = 1 - sqrt((get(all_bin[5]) - Nn)^2),
      SCORE = (So + Sc + Se + Sa + Sn)/5
    )
  return (d_z)
}
