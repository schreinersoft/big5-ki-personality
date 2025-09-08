data_aggregated %>% 
ggplot(aes(sample = o_llm)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", 
       x = "Theoretische Quantile", 
       y = "Stichproben-Quantile")+
  theme_minimal()

data_aggregated %>% 
  ggplot(aes(sample = c_llm)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal()

data_aggregated %>% 
  ggplot(aes(sample = e_llm)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", 
       x = "Theoretische Quantile", 
       y = "Stichproben-Quantile")+
  theme_minimal()

data_aggregated %>% 
  ggplot(aes(sample = a_llm)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", 
       x = "Theoretische Quantile", 
       y = "Stichproben-Quantile")+
  theme_minimal()

data_aggregated %>% 
  ggplot(aes(sample = n_llm)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", 
       x = "Theoretische Quantile", 
       y = "Stichproben-Quantile")+
  theme_minimal()

