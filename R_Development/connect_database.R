library(DBI)
library(dplyr)
library(dbplyr)
library(dotenv)

load_dot_env(file="../.env")

# Connect to PostgreSQL TEST
if (!exists("con")) 
{
con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "db.schreinersoft.de",
  port = 65432,
  dbname = "thesis-test",
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

}

# Connect to PostgreSQL
#con <- dbConnect(
#  RPostgreSQL::PostgreSQL(),
#  host = "db.schreinersoft.de",
#  port = 65432,
#  dbname = "thesis",
#  user = Sys.getenv("DB_USER"),
#  password = Sys.getenv("DB_PASSWORD")
#)

db_write_model <- function(data, model_version)
{
  model_version <- gsub("\\.", "", model_version)
  data_for_table <- data %>%
    select(
    essay_id,
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")) %>% 
    rename_with(~ c("essay_id",
                    paste("o_", model_version, sep=""),
                    paste("c_", model_version, sep=""),
                    paste("e_", model_version, sep=""),
                    paste("a_", model_version, sep=""),
                    paste("n_", model_version, sep="")
    ))
  table_name <- paste("model_", model_version, sep="")
  dbWriteTable(con, table_name, data_for_table, overwrite = TRUE, row.names = FALSE)
}

db_read_model <- function(model_version)
{
  model_version <- gsub("\\.", "", model_version)
  table_name <- paste("model_", model_version, sep="")
  data <- tbl(con, table_name) %>%
    collect()
  return (data)
}
