library(RPostgreSQL)

select = dplyr::select

con_params <- list(
  dbname   = "nrel-gis",
  host     = "localhost",
  port     = 5432,
  user     = "bbest",
  password = readLines("~/.nrel_db_pass"))
  # dbname   = "nreldb",
  # host     = "nreldb.cold0azgsoxa.us-west-1.rds.amazonaws.com",
  # port     = 5432,
  # user     = "bbest",
  # password = readLines("~/.aws_nreldb_pass"))

# connect to postgis database
con <- do.call(dbConnect, c(list(drv = PostgreSQL()), con_params))

#dbListTables(con)
