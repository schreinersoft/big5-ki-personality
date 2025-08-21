library(DBI)
library(dplyr)
library(dbplyr)
library(dotenv)

load_dot_env(file="../.env")

# Connect to PostgreSQL TEST
con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "db.schreinersoft.de",
  port = 65432,
  dbname = "thesis-test",
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Connect to PostgreSQL
#con <- dbConnect(
#  RPostgreSQL::PostgreSQL(),
#  host = "db.schreinersoft.de",
#  port = 65432,
#  dbname = "thesis",
#  user = Sys.getenv("DB_USER"),
#  password = Sys.getenv("DB_PASSWORD")
#)
