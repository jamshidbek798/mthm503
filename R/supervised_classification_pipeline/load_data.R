library(DBI)
library(RPostgres)
library(dplyr)

load_data <- function() {
  # Load environment variables
  readRenviron(".Renviron")
  con <- dbConnect(
    Postgres(),
    host = Sys.getenv("PGRHOST"),
    port = as.integer(Sys.getenv("PGRPORT")),
    user = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    dbname = Sys.getenv("PGRDATABASE")
  )

  casualties <- dbGetQuery(con, "SELECT * FROM stats19_casualties")
  accidents <- dbGetQuery(con, "SELECT * FROM stats19_accidents")
  vehicles <- dbGetQuery(con, "SELECT * FROM stats19_vehicles")

  dbDisconnect(con)

  list(
    casualties = casualties,
    accidents = accidents,
    vehicles = vehicles
  )
}
