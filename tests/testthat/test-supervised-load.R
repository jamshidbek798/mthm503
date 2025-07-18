test_that("load_data() returns clean casualty data", {
  skip_if_not(
    Sys.getenv("PGRHOST") != "" &&
      Sys.getenv("PGRPORT") != "" &&
      Sys.getenv("PGRUSER") != "" &&
      Sys.getenv("PGRPASSWORD") != "" &&
      Sys.getenv("PGRDATABASE") != "",
    "DB creds not set for CI; skipping DB‑dependent test."
  )

  df <- load_data()

  expect_s3_class(df, "data.frame")
  expect_true(all(c(
    "casualty_severity",
    "sex_of_casualty",
    "speed_limit_mph"
  ) %in% names(df)))
  expect_gt(nrow(df), 1000) # sanity: data not empty
  expect_false(any(is.na(df$casualty_severity))) # target column complete
})
