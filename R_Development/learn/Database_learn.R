library(DBI)
library(dplyr)
library(dbplyr)

# Connect to PostgreSQL
con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "db.schreinersoft.de",
  port = 65432,
  dbname = "thesis",
  user = "thesispg",
  password = "Post+gres"
)

# Write data to database
sample_data <- data.frame(
  id = 1:5,
  name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  age = c(25, 30, 35, 28, 32),
  salary = c(50000, 60000, 70000, 55000, 65000)
)

# Write data
dbWriteTable(con, "employees", sample_data, 
             overwrite = TRUE, row.names = FALSE)

employees_tbl <- tbl(con, "employees")
employees_tbl %>% select(name) %>% filter(name=="Alice")

dbListTables(con)

essays <- tbl(con, "essays")

glimpse(essays)

banjamin <- tbl(con, "benjamin")
glimpse(banjamin)

banjamin$text_raw_len <-  banjamin %>% pull(text_raw) %>% nchar()

banjamin %>% pull(year) %>% hist()

banjamin %>% 
  select(id, year) %>% 
  filter(year < 1940) %>% 
  pull(year) %>% 
  hist()

banjamin %>% 
  select(id, day) %>% 
  pull(day) %>% 
  hist(nclass=31)

