o_facets <- paste0("of", 1:6)
c_facets <- paste0("cf", 1:6)
e_facets <- paste0("ef", 1:6)
a_facets <- paste0("af", 1:6)
n_facets <- paste0("nf", 1:6)

# remove Items from analyzation
o_facets <- o_facets[o_facets != "of3"] 
o_facets <- o_facets[o_facets != "of4"]
c_facets <- c_facets[c_facets != "cf1"]
e_facets <- e_facets[e_facets != "ef1"]  
e_facets <- e_facets[e_facets != "ef3"]  
e_facets <- e_facets[e_facets != "ef6"]  
a_facets <- a_facets[a_facets != "af2"] 
a_facets <- a_facets[a_facets != "af5"]
n_facets <- n_facets[n_facets != "nf2"]  
n_facets <- n_facets[n_facets != "nf5"] 
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]

data_facets <- data_aggregated %>% 
  select(all_of(all_facets))

omega_result <- data_facets %>% 
  omega(nfactor = 5, flip=FALSE, fm ="ml")
omega_result

# schritt 1
# remove Items from analyzation
o_facets <- o_facets[o_facets != "of3"] 
o_facets <- o_facets[o_facets != "of4"]
c_facets <- c_facets[c_facets != "cf1"]
e_facets <- e_facets[e_facets != "ef1"]  
e_facets <- e_facets[e_facets != "ef6"]  
a_facets <- a_facets[a_facets != "af2"] 
a_facets <- a_facets[a_facets != "af5"]


e_facets <- e_facets[e_facets != "ef3"]  
n_facets <- n_facets[n_facets != "nf2"]  
n_facets <- n_facets[n_facets != "nf3"]  
n_facets <- n_facets[n_facets != "nf5"] 
all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]




omega_result <- data_facets %>% 
  omega(nfactor = 5, flip=FALSE)
omega_result




################### einzeln
data_o <- data_aggregated %>% 
  select(all_of(o_facets))
data_c <- data_aggregated %>% 
  select(all_of(c_facets))
data_e <- data_aggregated %>% 
  select(all_of(e_facets))
data_a <- data_aggregated %>% 
  select(all_of(a_facets))
data_n <- data_aggregated %>% 
  select(all_of(n_facets))


omega_o <- omega(data_o, flip=FALSE)
omega_c <- omega(data_c, flip=FALSE)
omega_e <- omega(data_e, flip=FALSE)
omega_a <- omega(data_a, flip=FALSE)
omega_n <- omega(data_n, flip=FALSE, plot=FALSE)


omega_n$omega.tot
omega_n$schmid$orthog
omega_n$schmid
omega_n$


############################################################# data_raw kein großer unterschied -> lassen!
#omega_result <- data_raw %>% 
#  omega(nfactor = 5, flip=FALSE)
#omega_result

for (facets in facet_list) {
  alpha_result <- data_facets %>% 
    select(all_of(facets)) %>% 
    alpha()
  
  print(alpha_result)
}
