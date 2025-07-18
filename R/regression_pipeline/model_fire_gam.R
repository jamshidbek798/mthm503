fire_gam1 <- function(data) {
  model <- vglm(
    extrication ~ sex + s(age_band_num, by = sex),
    family = multinomial,
    weights = n_casualties,
    data = data
  )
  print(summary(model))
  print(AIC(model))
  model
}
