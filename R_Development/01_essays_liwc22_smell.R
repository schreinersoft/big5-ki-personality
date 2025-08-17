library(tidyverse)
library(readr)

essays <- read_csv("00_cleaned/essays_cleaned.csv")

sum(essays$WC)
mean(essays$WC)
sd(essays$WC)

dbWriteTable(con, "essays", essays)
