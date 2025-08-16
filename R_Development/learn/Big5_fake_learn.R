library(tidyverse)

big <- read_csv("learn/big5-fake_comma.csv")

mean(big$O)

sd(big$O)

head(big)

znorm <- function(x){
  return (x-mean(x))/sd(x)
}

big$Oz <- znorm(big$O)
big$Cz <- znorm(big$C)
big$Ez <- znorm(big$E)
big$Az <- znorm(big$A)
big$Nz <- znorm(big$N)

big %>% ggplot(aes(x = Cz)) +
  geom_histogram(fill = "blue", color = "black", binwidth = 0.1) +
  labs(title = "Histogram of Values", x = "Values", y = "Frequency") +
  theme_minimal()

# Add fake binary coding
set.seed(42)  # For reproducibility
big$Obin <- sample(c(0, 1), size = 500, replace = TRUE)
big$Cbin <- sample(c(0, 1), size = 500, replace = TRUE)
big$Ebin <- sample(c(0, 1), size = 500, replace = TRUE)
big$Abin <- sample(c(0, 1), size = 500, replace = TRUE)
big$Nbin <- sample(c(0, 1), size = 500, replace = TRUE)

head(big)

threshold_select <- function(data, threshold){
  return(
    ((data$Obin == 1 & data$Oz >= threshold) | (data$Obin == 0 & data$Oz < -threshold)) &
      ((data$Cbin == 1 & data$Cz >= threshold) | (data$Cbin == 0 & data$Cz < -threshold)) &
      ((data$Ebin == 1 & data$Ez >= threshold) | (data$Ebin == 0 & data$Ez < -threshold)) &
      ((data$Abin == 1 & data$Az >= threshold) | (data$Abin == 0 & data$Az < -threshold)) &
      ((data$Nbin == 1 & data$Nz >= threshold) | (data$Nbin == 0 & data$Nz < -threshold))) * 1
}

big$sel = threshold_select(big, 0.05)
head(big)

# Ermitteln eines Thresholds

thresholds <- tibble(thrds = seq(0, 0.3, by = 0.01))

thresholds$n <- purrr::map_int(thresholds$thrds, ~{
  selected_rows <- threshold_select(big, .x)
  sum(selected_rows == 1)
})

thresholds %>% ggplot(aes(x=thrds, y=n)) +
  geom_col(fill = "blue", color = "black") +
  labs(title = "Threshold vs. Correct Estimates", x = "Threshold", y = "Correct Estimates") +
  theme_minimal()

