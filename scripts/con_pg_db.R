library(RPostgreSQL)

select = dplyr::select

con_params <- list(
  dbname   = "nrel-gis",
  host     = "localhost",
  port     = 5432,
  user     = "bbest",
  password = readLines("~/.nrel_db_pass"))

# connect to postgis database
con <- do.call(dbConnect, c(list(drv = PostgreSQL()), con_params))

#dbListTables(con)
