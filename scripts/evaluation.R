# Load necessary libraries
library(caret)
library(readr)
library(pROC)
library(ROCR)
library(ggplot2)

# Load the test data
testData <- read_csv("./data/processed/cleaned_data.csv")
testData$Outcome <- as.factor(testData$Outcome)  # Ensure Outcome is a factor for classification

# Load classification models
classification_models <- list(
  logistic = readRDS("./outputs/models/classification/logistic_model.rds"),
  svm = readRDS("./outputs/models/classification/svm_model.rds"),
  nb = readRDS("./outputs/models/classification/nb_model.rds"),
  xgb = readRDS("./outputs/models/classification/xgb_model.rds")
)

# Load regression models
regression_models <- list(
  lm = readRDS("./outputs/models/regression/lm_model.rds"),
  rf = readRDS("./outputs/models/regression/rf_model.rds"),
  xgb = readRDS("./outputs/models/regression/xgb_model.rds")
)

# Function to evaluate classification models
evaluate_classification_model <- function(model, data, model_name) {
  predictions <- predict(model, data, type = "prob")
  predicted_classes <- ifelse(predictions[,2] > 0.5, 1, 0)
  
  # Confusion matrix and derived metrics
  confMatrix <- confusionMatrix(as.factor(predicted_classes), data$Outcome)
  sensitivity <- confMatrix$byClass['Sensitivity']
  specificity <- confMatrix$byClass['Specificity']
  accuracy <- confMatrix$overall['Accuracy']
  
  # ROC Curve and AUC
  rocResult <- roc(response = data$Outcome, predictor = predictions[,2])
  auc_value <- auc(rocResult)
  rocPlot <- plot(rocResult, main = paste("ROC Curve for", model_name), col = "blue", lwd = 2)
  
  # Precision-Recall Curve
  predObj <- prediction(predictions[,2], data$Outcome)
  prCurve <- performance(predObj, "prec", "rec")
  prPlot <- plot(prCurve, main = paste("Precision-Recall Curve for", model_name), col = "green", lwd = 2)
  
  # Display detailed metrics and plots
  cat("\nClassification Metrics for", model_name, ":\n")
  print(confMatrix)
  cat("Accuracy:", accuracy, "\n")
  cat("Sensitivity:", sensitivity, "\n")
  cat("Specificity:", specificity, "\n")
  cat("AUC:", auc_value, "\n")
  
  return(list(ConfusionMatrix = confMatrix, Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, AUC = auc_value))
}

# Function to evaluate regression models
evaluate_regression_model <- function(model, data, model_name) {
  predictions <- predict(model, data)
  residuals <- data$BMI - predictions
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  r_squared <- if ("lm" %in% model$method || "rf" %in% model$method) summary(model$finalModel)$r.squared else NA
  
  # Residual Plot
  residualPlot <- ggplot(data, aes(predictions, residuals)) +
    geom_point(color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residual Plot for", model_name), x = "Predicted Values", y = "Residuals") +
    theme_minimal()
  ggsave(filename = paste0("./outputs/images/residual_plot_", model_name, ".png"), plot = residualPlot)
  
  cat("\nRegression Metrics for", model_name, ":\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("R-squared:", r_squared, "\n")
  
  return(list(RMSE = rmse, MAE = mae, R_squared = r_squared))
}

# Evaluate classification models
cat("\n--- Classification Model Evaluations ---\n")
classification_results <- lapply(names(classification_models), function(name) evaluate_classification_model(classification_models[[name]], testData, name))

# Evaluate regression models
cat("\n--- Regression Model Evaluations ---\n")
regression_results <- lapply(names(regression_models), function(name) evaluate_regression_model(regression_models[[name]], testData, name))

# Print summary results for classification
cat("\n--- Classification Results Summary ---\n")
for (result in names(classification_results)) {
  cat("\nModel:", result, "\n")
  print(classification_results[[result]])
}

# Print summary results for regression
cat("\n--- Regression Results Summary ---\n")
for (result in names(regression_results)) {
  cat("\nModel:", result, "\n")
  print(regression_results[[result]])
}
