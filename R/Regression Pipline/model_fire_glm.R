fire_glm1 <- function(data) {
  model <- vglm(
    extrication ~ age_band + sex,
    family = multinomial,
    weights = n_casualties,
    data = data
  )
  print(summary(model))
  print(AIC(model))
  model
}

fire_glm2 <- function(data) {
  model <- vglm(
    extrication ~ age_band + sex + age_band * sex,
    family = multinomial,
    weights = n_casualties,
    data = data
  )
  print(summary(model))
  print(AIC(model))
  model
}
