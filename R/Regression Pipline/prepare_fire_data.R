prepare_fire_data <- function(data) {
  data %>%
    filter(
      extrication != "Unknown",
      sex != "Unknown",
      age_band != "Unknown",
      n_casualties > 0
    ) %>%
    mutate(
      age_band = factor(age_band, levels = c("0-16", "17-24", "25-39", "40-64", "65+"), ordered = TRUE),
      age_band_num = as.numeric(age_band),
      sex = factor(sex),
      extrication = factor(extrication),
      year_numeric = as.numeric(sub("/.*", "", financial_year))
    )
}
