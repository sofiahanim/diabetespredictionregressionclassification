# Load necessary libraries
library(caret)
library(randomForest)
library(xgboost)
library(readr)

# Load the processed data
data <- read_csv("./data/processed/cleaned_data.csv")

# Exclude the Outcome variable for regression tasks
dataReg <- data[, !names(data) %in% c("Outcome")]

# Splitting data into training and testing
set.seed(123)
trainIndex <- createDataPartition(dataReg$BMI, p = 0.8, list = FALSE)
trainData <- dataReg[trainIndex, ]
testData <- dataReg[-trainIndex, ]

# Train Linear Regression Model
lm_model <- train(
  BMI ~ ., 
  data = trainData, 
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    summaryFunction = defaultSummary
  )
)

# Train Random Forest Regression Model with hyperparameter tuning
rf_grid <- expand.grid(.mtry = seq(2, ncol(trainData) - 1, by = 1))
rf_model <- train(
  BMI ~ ., 
  data = trainData, 
  method = "rf",
  tuneGrid = rf_grid,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    summaryFunction = defaultSummary
  )
)

# Train XGBoost Regression Model with hyperparameter tuning
xgb_grid <- expand.grid(
  nrounds = seq(50, 200, by = 50),
  lambda = c(0.01, 0.1, 1),
  alpha = c(0, 0.1, 1),
  eta = c(0.01, 0.1, 0.3)
)
xgb_model <- train(
  BMI ~ ., 
  data = trainData, 
  method = "xgbLinear",
  tuneGrid = xgb_grid,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    summaryFunction = defaultSummary
  )
)

# Save models
saveRDS(lm_model, "./outputs/models/regression/lm_model.rds")
saveRDS(rf_model, "./outputs/models/regression/rf_model.rds")
saveRDS(xgb_model, "./outputs/models/regression/xgb_model.rds")
