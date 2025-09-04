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

omega_o <- omega(data_o, flip=FALSE, plot=FALSE)
omega_c <- omega(data_c, flip=FALSE, plot=FALSE)
omega_e <- omega(data_e, flip=FALSE, plot=FALSE)
omega_a <- omega(data_a, flip=FALSE, plot=FALSE)
omega_n <- omega(data_n, flip=FALSE, plot=FALSE)
fa_o <- fa(data_o)
fa_c <- fa(data_c)
fa_e <- fa(data_e)
fa_a <- fa(data_n)
fa_n <- fa(data_o)
alpha_o <- alpha(data_o)
alpha_c <- alpha(data_c)
alpha_e <- alpha(data_e)
alpha_a <- alpha(data_n)
alpha_n <- alpha(data_o)



omega_results <- tibble(
  factor = c("O", "C", "E", "A", "N", "Mean"),
  alpha = c(
    round(alpha_o$total$raw_alpha, 2),
    round(alpha_c$total$raw_alpha, 2),
    round(alpha_e$total$raw_alpha, 2),
    round(alpha_a$total$raw_alpha, 2),
    round(alpha_n$total$raw_alpha, 2),
    round(mean(c(alpha_o$total$raw_alpha, 
                 alpha_c$total$raw_alpha, 
                 alpha_e$total$raw_alpha,
                 alpha_a$total$raw_alpha,
                 alpha_n$total$raw_alpha)), 2)),
  omega = c(
    round(omega_o$omega.tot, 2),
    round(omega_c$omega.tot, 2),
    round(omega_e$omega.tot, 2),
    round(omega_a$omega.tot, 2),
    round(omega_n$omega.tot, 2),
    round(mean(c(omega_o$omega.tot, 
            omega_c$omega.tot,
            omega_e$omega.tot,
            omega_a$omega.tot,
            omega_n$omega.tot)),2))
)

print(omega_results)
print(alpha_o)
print(omega_o)
print(fa_o)

print(alpha_c)
print(omega_c)
print(fa_c)

print(alpha_e)
print(omega_e)
print(fa_e)

print(alpha_a)
print(omega_a)
print(fa_a)

print(alpha_n)
print(omega_n)
print(fa_n)


