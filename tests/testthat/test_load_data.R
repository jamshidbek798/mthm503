source(here::here("R", "load_data.R"))

test_that("mocked data is returned with CI", {
  withr::with_envvar(c(CI = "true"), {
    df <- load_data()
    expect_true(is.data.frame(df))
    expect_named(df, c("casualty_severity", "sex_of_casualty"))
  })
})
