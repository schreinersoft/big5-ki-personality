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

