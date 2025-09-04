# create aggregated results of llm calculations for 6 facets
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