# Load necessary libraries
library(caret)
library(e1071)
library(naivebayes)
library(xgboost)
library(readr)

# Load the processed data
data <- read_csv("./data/processed/cleaned_data.csv")

# Ensure the Outcome variable is a factor for classification
data$Outcome <- as.factor(data$Outcome)

# Splitting data into training and testing
set.seed(123)
trainIndex <- createDataPartition(data$Outcome, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train Logistic Regression Model
logistic_model <- train(
  Outcome ~ ., 
  data = trainData, 
  method = "glm", 
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary
  )
)

# Train SVM Model with hyperparameter tuning
svm_grid <- expand.grid(.C = c(0.1, 1, 10), .sigma = c(0.01, 0.1, 1))
svm_model <- train(
  Outcome ~ ., 
  data = trainData, 
  method = "svmRadial",
  tuneGrid = svm_grid,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary
  )
)

# Train Naive Bayes Model
nb_model <- naiveBayes(Outcome ~ ., data = trainData)

# Train XGBoost Model with hyperparameter tuning
xgb_grid <- expand.grid(
  nrounds = seq(50, 200, by = 50),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.8, 1),
  min_child_weight = c(1, 5)
)
xgb_model <- train(
  Outcome ~ ., 
  data = trainData, 
  method = "xgbTree",
  tuneGrid = xgb_grid,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary
  )
)

# Save models
saveRDS(logistic_model, "./outputs/models/classification/logistic_model.rds")
saveRDS(svm_model, "./outputs/models/classification/svm_model.rds")
saveRDS(nb_model, "./outputs/models/classification/nb_model.rds")
saveRDS(xgb_model, "./outputs/models/classification/xgb_model.rds")
