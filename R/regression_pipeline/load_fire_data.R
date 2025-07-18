load_fire_data <- function() {
  readRenviron(".Renviron")
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("PGRHOST"),
    port     = as.integer(Sys.getenv("PGRPORT")),
    user     = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    dbname   = Sys.getenv("PGRDATABASE")
  )
  fire <- dbGetQuery(con, "SELECT * FROM fire_rescue_extrication_casualties")
  dbDisconnect(con)
  fire
}
