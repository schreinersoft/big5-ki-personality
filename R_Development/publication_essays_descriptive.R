library(tidyverse)
library(psych)
library(flextable)

source("connect_database.R")
source("functions.R")
source("Factor-Names-EN.R")

essays <- tbl(con, "essays")  %>% select(-text) %>% collect()
essays$o_bin <- ifelse(essays$o_binary == "1", 1, 0)
essays$c_bin <- ifelse(essays$c_binary == "1", 1, 0)
essays$e_bin <- ifelse(essays$e_binary == "1", 1, 0)
essays$a_bin <- ifelse(essays$a_binary == "1", 1, 0)
essays$n_bin <- ifelse(essays$n_binary == "1", 1, 0)


summ <- essays %>% 
  summarise(
    n = n(),
    O = sum(o_bin),
    C = sum(c_bin),
    E = sum(e_bin),
    A = sum(a_bin),
    N = sum(n_bin),
    Op = O / n,
    Cp = C / n,
    Ep = E / n,
    Ap = A / n,
    Np = N / n,
    
  )
summ

essays250 <- essays %>% filter(id <= 250)

summ250 <- essays250 %>% 
  summarise(
    n = n(),
    O = sum(o_bin),
    Op = O / n,
    C = sum(c_bin),
    Cp = C / n,
    E = sum(e_bin),
    Ep = E / n,
    A = sum(a_bin),
    Ap = A / n,
    N = sum(n_bin),
    Np = N / n,
  )
summ250


psych_table <- summ %>%
  flextable() %>%
  set_header_labels(
    Variable = "Datensatz",
    n = "N",
    O = "O",
    OP = "Op",
    C = "C",
    E = "E",
    A = "A",
    N = "N"
  ) %>%
  theme_vanilla() %>%
  autofit() %>%
  align(j = 3:6, align = "center", part = "all")
psych_table

