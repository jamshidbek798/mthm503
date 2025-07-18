library(caret)
library(dplyr)
library(pROC)
library(tibble)

train_models <- function(prepared_data) {
  set.seed(42)

  train_idx <- createDataPartition(prepared_data$casualty_severity, p = 0.8, list = FALSE)
  train_data <- prepared_data[train_idx, ]
  test_data <- prepared_data[-train_idx, ]

  cv_ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = multiClassSummary,
    savePredictions = TRUE,
    sampling = "smote"
  )

  model_multinom <- train(
    casualty_severity ~ .,
    data = train_data,
    method = "multinom", trControl = cv_ctrl, trace = FALSE
  )

  model_rf <- train(
    casualty_severity ~ .,
    data = train_data,
    method = "rf", trControl = cv_ctrl, importance = TRUE, ntree = 100
  )

  model_xgb <- train(
    casualty_severity ~ .,
    data = train_data,
    method = "xgbTree", trControl = cv_ctrl
  )

  list(
    models = list(multinom = model_multinom, rf = model_rf, xgb = model_xgb),
    test_data = test_data
  )
}

evaluate_models <- function(models_list) {
  test_data <- models_list$test_data
  models <- models_list$models
  actual <- test_data$casualty_severity

  predictions <- lapply(models, predict, newdata = test_data)
  probs <- lapply(models, predict, newdata = test_data, type = "prob")

  accuracy_results <- tibble(
    Model = names(predictions),
    Accuracy = sapply(predictions, function(pred) MLmetrics::Accuracy(pred, actual))
  )

  aucs <- sapply(probs, function(prob_mat) {
    multiclass.roc(actual, prob_mat)$auc
  })

  roc_auc_results <- tibble(
    Model = names(aucs),
    Multiclass_AUC = aucs
  )

  final_results <- left_join(accuracy_results, roc_auc_results, by = "Model")

  # Confusion matrices
  conf_matrices <- lapply(predictions, function(pred) confusionMatrix(pred, actual))

  list(
    results = final_results,
    confusion_matrices = conf_matrices,
    roc_data = list(probs = probs, actual = actual)
  )
}
