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
library(tidyr)
library(knitr)


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

# Query the stats19_casualties table
stats19_casualties <- dbGetQuery(con, "SELECT * FROM stats19_casualties")
stats19_accidents <- dbGetQuery(con, "SELECT * FROM stats19_accidents")
stats19_vehicles <- dbGetQuery(con, "SELECT * FROM stats19_vehicles")

# Disconnect when done
dbDisconnect(con)



# Prepare data  -----------------------------------------------------------

data <- stats19_casualties %>%
  filter(casualty_class == "Pedestrian") |>
  left_join(stats19_accidents, by = "accident_index") %>%
  left_join(stats19_vehicles, by = c("accident_index", "vehicle_reference"), relationship = "many-to-many")


data <- data |>
  mutate(across(
    everything(),
    ~ ifelse(. == -1, NA, .)
  )) |>
  mutate(across(
    where(~ is.character(.) | is.factor(.)),
    ~ na_if(trimws(as.character(.)), "Unknown")
  )) |>
  mutate(across(
    where(~ is.character(.) | is.factor(.)),
    ~ na_if(., "Not known")
  ))

data <- data %>%
  select(
    # Target variable
    casualty_severity,

    # Crash characteristics
    obs_date,
    light_conditions,
    weather_conditions,
    road_surface_conditions,
    special_conditions_at_site,
    carriageway_hazards,
    speed_limit_mph,
    road_type,
    junction_detail,
    junction_control,
    pedestrian_crossing_human_control,
    pedestrian_crossing_physical_facilities,
    pedestrian_location,
    pedestrian_movement,

    # Casualty details
    sex_of_casualty,
    age_of_casualty,
    casualty_home_area_type,

    # Driver details
    sex_of_driver,
    age_of_driver,
    driver_home_area_type,

    # Vehicle & road usage
    vehicle_type,
    vehicle_manoeuvre
  )


ggplot(data, aes(x = as.factor(casualty_severity), fill = as.factor(casualty_severity))) +
  geom_bar() +
  labs(title = "Casualty Severity Distribution", x = "Severity", y = "Count", fill = "Casualty Severity") +
  theme_minimal()


# Select numeric columns
num_vars <- data %>% select(where(is.numeric))

# Calculate correlation matrix (exclude NA values)
corr_matrix <- cor(num_vars, use = "pairwise.complete.obs")

# Plot correlation matrix
ggcorrplot(corr_matrix, lab = T)
corrplot(corr_matrix)





# Missingness map
vis_miss(data, sort_miss = T) +
  theme(axis.text.x = element_text(angle = 89))


# Drop columns with high missingness
data <- data %>%
  select(where(~ mean(is.na(.)) < 0.5)) # keeps columns with < 50% missing

vis_miss(data)


data <- data %>%
  mutate(across(where(is.character), as.factor))

imputed <- mice(data, method = "cart", m = 5)

data <- complete(imputed)

vis_miss(data)



data <- data |>
  mutate(
    obs_date_parsed = as_datetime(as.numeric(obs_date), tz = "UTC"),
    weekday = wday(obs_date_parsed, label = TRUE),
    hour = hour(obs_date_parsed),
    month = month(obs_date_parsed, label = TRUE),
    year = year(obs_date_parsed),

    # Simplify light conditions into 3 categories
    light_simple = case_when(
      light_conditions == "Daylight" ~ "Daylight",
      light_conditions == "Darkness - lights lit" ~ "Lit Darkness",
      TRUE ~ "Unlit Darkness" # includes no lighting, lighting unknown, lights unlit
    ),
    weather_simple = case_when(
      grepl("^Fine", weather_conditions) ~ "Clear",
      grepl("^Rain", weather_conditions) | grepl("Fog", weather_conditions) ~ "Rain",
      grepl("^Snow", weather_conditions) ~ "Snow",
      weather_conditions %in% c("Other", "Unknown") ~ "Other",
      TRUE ~ "Unknown"
    ),
    road_surface_simple = case_when(
      road_surface_conditions == "Dry" ~ "Dry",
      road_surface_conditions == "Wet or damp" ~ "Wet",
      road_surface_conditions %in% c("Frost or ice", "Snow", "Flood over 3cm. deep") ~ "Slippery",
      TRUE ~ "Unknown"
    ),
    carriageway_hazard_simple = case_when(
      carriageway_hazards == "None" ~ "None",
      carriageway_hazards == "Pedestrian in carriageway - not injured" ~ "Pedestrian Hazard",
      TRUE ~ "Object Hazard" # includes "Other object on road", "Vehicle load on road"
    ),
    speed_category = case_when(
      speed_limit_mph <= 30 ~ "Low",
      speed_limit_mph <= 50 ~ "Medium",
      speed_limit_mph > 50 ~ "High"
    ),
    casualty_age_group = case_when(
      age_of_casualty <= 15 ~ "Child",
      age_of_casualty <= 24 ~ "Youth",
      age_of_casualty <= 44 ~ "Adult",
      age_of_casualty <= 64 ~ "Middle-aged",
      age_of_casualty >= 65 ~ "Senior"
    ),
    home_area_binary = case_when(
      casualty_home_area_type == "Urban area" ~ "Urban",
      casualty_home_area_type %in% c("Small town", "Rural") ~ "Non-urban"
    ),
    driver_age_group = case_when(
      age_of_driver <= 24 ~ "Young",
      age_of_driver <= 39 ~ "Adult",
      age_of_driver <= 59 ~ "Middle-aged",
      age_of_driver <= 74 ~ "Senior",
      age_of_driver >= 75 ~ "Elderly"
    )
  )

colnames(data)

drop_vars <- c(
  "obs_date_parsed", "obs_date", "light_conditions",
  "weather_conditions", "road_surface_conditions",
  "carriageway_hazards", "speed_limit_mph",
  "age_of_casualty", "casualty_home_area_type", "age_of_driver"
)

data <- data |>
  select(-all_of(drop_vars))

data <- data |>
  mutate(across(where(is.character), as.factor))

# Set factor levels first
data$speed_category <- factor(data$speed_category, levels = c("Low", "Medium", "High"))

# Now plot
p1 <- ggplot(data, aes(x = speed_category, fill = casualty_severity)) +
  geom_bar(position = "dodge") +
  ggtitle("Speed Zone vs Severity")


p2 <- ggplot(data, aes(light_simple, fill = casualty_severity)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c("Day", "Lit Darkness", "Unlit Darkness")) +
  ggtitle("Night vs Severity")

p3 <- ggplot(data, aes(factor(road_surface_simple, levels = c("Dry", "Wet", "Slippery")),
  fill = casualty_severity
)) +
  geom_bar(position = "dodge") +
  ggtitle("Road Surface condition vs Severity") +
  xlab("Road Surface Condition")

p4 <- ggplot(data, aes(factor(casualty_age_group, levels = c("Child", "Youth", "Adult", "Middle-aged", "Senior")),
  fill = casualty_severity
)) +
  geom_bar(position = "dodge") +
  ggtitle("Casualty Age Group vs Severity") +
  xlab("Casualty Age Group")

gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)





# Test --------------------------------------------------------------------


# Set seed for reproducibility
set.seed(42)

data_selected <- data

# 1. Split data into train/test (80/20)
train_idx <- createDataPartition(data_selected$casualty_severity, p = 0.8, list = FALSE)
train_data <- data_selected[train_idx, ]
test_data <- data_selected[-train_idx, ]

# 2. Set up 5-fold CV with SMOTE (for imbalanced classes)
cv_ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = TRUE,
  sampling = "smote"
)

# 3. Train multinomial logistic regression
model_multinom <- train(
  casualty_severity ~ .,
  data = train_data,
  method = "multinom",
  trControl = cv_ctrl,
  trace = FALSE
)

# 4. Train random forest
model_rf <- train(
  casualty_severity ~ .,
  data = train_data,
  method = "rf",
  trControl = cv_ctrl,
  importance = TRUE,
  ntree = 100
)

# 5. Train XGBoost
model_xgb <- train(
  casualty_severity ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = cv_ctrl
)

# Predict
pred_multinom <- predict(model_multinom, newdata = test_data)
pred_rf <- predict(model_rf, newdata = test_data)
pred_xgb <- predict(model_xgb, newdata = test_data)

# Actual
actual <- test_data$casualty_severity



accuracy_results <- tibble(
  Model = c("Multinomial Logistic", "Random Forest", "XGBoost"),
  Accuracy = c(
    Accuracy(pred_multinom, actual),
    Accuracy(pred_rf, actual),
    Accuracy(pred_xgb, actual)
  )
)

print(accuracy_results)


conf_matrix_multinom <- confusionMatrix(pred_multinom, actual)
conf_matrix_rf <- confusionMatrix(pred_rf, actual)
conf_matrix_xgb <- confusionMatrix(pred_xgb, actual)

# Print one-by-one or save:
print(conf_matrix_multinom)
print(conf_matrix_rf)
print(conf_matrix_xgb)


get_metrics <- function(pred, actual) {
  cm <- confusionMatrix(pred, actual)
  as.data.frame(cm$byClass)[, c("Precision", "Recall", "F1")]
}

metrics_multinom <- get_metrics(pred_multinom, actual)
metrics_rf <- get_metrics(pred_rf, actual)
metrics_xgb <- get_metrics(pred_xgb, actual)

# Combine into one table
bind_rows(
  metrics_multinom %>% mutate(Model = "Multinomial"),
  metrics_rf %>% mutate(Model = "Random Forest"),
  metrics_xgb %>% mutate(Model = "XGBoost")
) %>%
  relocate(Model)


# Need one-vs-rest probabilities
probs_multinom <- predict(model_multinom, newdata = test_data, type = "prob")
probs_rf <- predict(model_rf, newdata = test_data, type = "prob")
probs_xgb <- predict(model_xgb, newdata = test_data, type = "prob")

# Convert actual to factor with consistent levels
actual_factor <- factor(actual, levels = levels(pred_multinom))

# ROC AUC for each class
auc_multinom <- multiclass.roc(actual_factor, probs_multinom)$auc
auc_rf <- multiclass.roc(actual_factor, probs_rf)$auc
auc_xgb <- multiclass.roc(actual_factor, probs_xgb)$auc

roc_auc_results <- tibble(
  Model = c("Multinomial Logistic", "Random Forest", "XGBoost"),
  Multiclass_AUC = c(auc_multinom, auc_rf, auc_xgb)
)

print(roc_auc_results)

final_results <- left_join(accuracy_results, roc_auc_results, by = "Model")
print(final_results)

library(tidyr)

# Function to convert confusion matrix to data frame
conf_to_df <- function(cm, model_name) {
  df <- as.data.frame(cm$table)
  names(df) <- c("Reference", "Prediction", "Freq")
  df$Model <- model_name
  df
}

df_cm <- bind_rows(
  conf_to_df(conf_matrix_multinom, "Multinomial"),
  conf_to_df(conf_matrix_rf, "Random Forest"),
  conf_to_df(conf_matrix_xgb, "XGBoost")
)

ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  facet_wrap(~Model, ncol = 1) +
  labs(title = "Confusion Matrix Heatmaps", x = "Actual", y = "Predicted") +
  theme_minimal()



library(pROC)
# Get actual classes and predicted probabilities
actual <- factor(actual, levels = levels(pred_multinom)) # Ensure factor with right levels

# Function to compute one-vs-rest ROC curves and extract AUC + coords
get_roc_data <- function(probs, model_name) {
  res <- list()
  for (class in colnames(probs)) {
    bin_actual <- ifelse(actual == class, 1, 0)
    roc_obj <- pROC::roc(bin_actual, probs[[class]], quiet = TRUE)

    coords_df <- as.data.frame(pROC::coords(roc_obj)) # force pROC version
    names(coords_df) <- c("threshold", "sensitivity", "specificity")

    coords_df$Model <- model_name
    coords_df$Class <- class
    coords_df$AUC <- as.numeric(pROC::auc(roc_obj))

    res[[class]] <- coords_df
  }
  dplyr::bind_rows(res)
}


# Compute for each model
roc_multinom_df <- get_roc_data(probs_multinom, "Multinomial")
roc_rf_df <- get_roc_data(probs_rf, "Random Forest")
roc_xgb_df <- get_roc_data(probs_xgb, "XGBoost")

# Combine all
roc_all <- bind_rows(roc_multinom_df, roc_rf_df, roc_xgb_df)

# Plot ROC curves: Sensitivity vs 1 - Specificity
ggplot(roc_all, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line() +
  facet_wrap(~Class, ncol = 3) +
  labs(
    title = "Multiclass ROC Curves (One-vs-Rest)",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  # theme_minimal() +
  theme(legend.position = "bottom")



# 10. Multiclass ROC / AUC
prob_multinom <- predict(model_multinom, test_data, type = "prob")
prob_rf <- predict(model_rf, test_data, type = "prob")
prob_xgb <- predict(model_xgb, test_data, type = "prob")

auc_multinom <- multiclass.roc(test_data$casualty_severity, prob_multinom)
auc_rf <- multiclass.roc(test_data$casualty_severity, prob_rf)
auc_xgb <- multiclass.roc(test_data$casualty_severity, prob_xgb)

print(paste("Multinom AUC:", auc_multinom$auc))
print(paste("Random Forest AUC:", auc_rf$auc))
print(paste("XGBoost AUC:", auc_xgb$auc))
