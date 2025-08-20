library("tidyverse")

essays <- tbl(con, "essays") %>% collect()

# Convert factor to binary (1 for "Yes", 0 for "No")
essays$o_bin <- ifelse(essays$o_binary == "1", 1, 0)

