# create aggregated results of llm calculations for 6 facets and Temperature
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
    of4 = mean(of4, na.rm = TRUE),
    cf4 = mean(cf4, na.rm = TRUE),
    ef4 = mean(ef4, na.rm = TRUE),
    af4 = mean(af4, na.rm = TRUE),
    nf4 = mean(nf4, na.rm = TRUE),
    of5 = mean(of5, na.rm = TRUE),
    cf5 = mean(cf5, na.rm = TRUE),
    ef5 = mean(ef5, na.rm = TRUE),
    af5 = mean(af5, na.rm = TRUE),
    nf5 = mean(nf5, na.rm = TRUE),
    of6 = mean(of6, na.rm = TRUE),
    cf6 = mean(cf6, na.rm = TRUE),
    ef6 = mean(ef6, na.rm = TRUE),
    af6 = mean(af6, na.rm = TRUE),
    nf6 = mean(nf6, na.rm = TRUE),
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE),
        .groups = "drop") 

data_facets <- data_aggregated %>% 
  select(all_of(all_facets)) %>% 
  as_tibble()