load_oil_data <- function() {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "postgres",
    host     = "aws-0-eu-west-2.pooler.supabase.com",
    user     = "pgstudent.rvdwflidqvcvffdccwrh",
    password = "0%jkXK^tjMZwuG",
    port     = 5432
  )

  oil_df <- dplyr::tbl(con, "olive_oil") |>
    dplyr::collect() |>
    dplyr::select(-id)

  DBI::dbDisconnect(con)
  return(oil_df)
}
