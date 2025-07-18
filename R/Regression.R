library(DBI)
library(RPostgres)
library(tidyverse)
library(caret)
library(lubridate)
library(corrplot)
library(randomForest)
library(nnet)
library(pROC)
library(car)
library(VIM)
library(gridExtra)
library(MLmetrics)
library(xgboost)
library(ggcorrplot)
library(naniar)
library(mice)
library(fastDummies)
library(gratia)
library(VGAM)


# Connect to database and load dataset ------------------------------------

readRenviron(".Renviron")

# Get connection details from environment
host <- Sys.getenv("PGRHOST")
port <- as.integer(Sys.getenv("PGRPORT"))
user <- Sys.getenv("PGRUSER")
password <- Sys.getenv("PGRPASSWORD")
dbname <- Sys.getenv("PGRDATABASE")

# Create connection to the database
con <- dbConnect(
  Postgres(),
  host = host,
  port = port,
  user = user,
  password = password,
  dbname = dbname
)

dbListTables(con)

# Query the stats19_casualties table
fire <- dbGetQuery(con, "SELECT * FROM fire_rescue_extrication_casualties")
finance <- dbGetQuery(con, "SELECT * FROM stats19_by_financial_year")

# Disconnect when done
dbDisconnect(con)



colnames(fire)
colnames(finance)

unique(fire$extrication)




# Prepare data: remove "Unknown" and missing values
fire_gam_data <- fire %>%
  filter(
    extrication != "Unknown",
    sex != "Unknown",
    age_band != "Unknown"
  ) %>%
  mutate(
    # Define age_band as an ordered factor with correct order
    age_band = factor(age_band,
      levels = c("0-16", "17-24", "25-39", "40-64", "65+"),
      ordered = TRUE
    ),

    # Convert ordered age_band to numeric for smooth term
    age_band_num = as.numeric(age_band),

    # Convert sex and extrication to factors
    sex = factor(sex),
    extrication = factor(extrication)
  )



fire_gam_data <- fire_gam_data %>%
  filter(n_casualties > 0)
# Optional: drop rare categories if very small



# Fit multinomial GAM
fire_gam_data$year_numeric <- as.numeric(sub("/.*", "", fire_gam_data$financial_year))

QQ <- function(model) {
  # Deviance residuals
  resid_dev <- residuals(model, type = "pearson")

  # QQ plot
  qqnorm(resid_dev, main = "QQ Plot of Pearson Residuals")
  qqline(resid_dev, col = "red")
}

# GLM 1 -------------------------------------------------------------------

glm_model1 <- vglm(
  extrication ~ age_band + sex,
  family = multinomial,
  weights = n_casualties,
  data = fire_gam_data
)
summary(glm_model1)
AIC(glm_model1)

QQ(glm_model1)

# GLM 2 -------------------------------------------------------------------

glm_model2 <- vglm(
  extrication ~ age_band + sex + age_band * sex,
  family = multinomial,
  weights = n_casualties,
  data = fire_gam_data
)

QQ(glm_model2)
summary(glm_model2)
AIC(glm_model2)


# GAM 2 -------------------------------------------------------------------



fire_gam_data

gam_model1 <- vglm(
  extrication ~
    sex + s(age_band_num, by = sex), # smooth for year
  family = multinomial,
  data = fire_gam_data,
  weights = n_casualties
)

QQ(gam_model1)
summary(gam_model1)
AIC(gam_model1)
