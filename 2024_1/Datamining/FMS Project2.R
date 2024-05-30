# Load necessary libraries
library(tidyverse)

# Load the dataset
dataset <- read.csv("FMS2.csv")

# Display the first few rows of the dataset
head(dataset)

# Check for missing values
sum(is.na(dataset))

# Convert 'person' and 'label' to factors
dataset$person <- as.factor(dataset$person)
dataset$label <- as.factor(dataset$label)

# Summary statistics
summary(dataset)

# Visualize distribution of x_pos, y_pos, z_pos
ggplot(dataset, aes(x = x_pos)) + geom_histogram(binwidth = 1) + ggtitle("Distribution of x_pos")
ggplot(dataset, aes(x = y_pos)) + geom_histogram(binwidth = 1) + ggtitle("Distribution of y_pos")
ggplot(dataset, aes(x = z_pos)) + geom_histogram(binwidth = 1) + ggtitle("Distribution of z_pos")

#Feature Engineering
library(dplyr)

# Convert log_time to numeric
dataset <- dataset %>%
  mutate(log_time = as.numeric(sub(":", "", log_time)))

# Calculate speed and delta features
dataset <- dataset %>%
  arrange(person, log_time) %>%
  group_by(person) %>%
  mutate(
    delta_x = c(0, diff(x_pos)),
    delta_y = c(0, diff(y_pos)),
    delta_z = c(0, diff(z_pos)),
    speed = sqrt(delta_x^2 + delta_y^2 + delta_z^2) / c(1, diff(log_time))
  ) %>%
  ungroup()

# Check the new features
head(dataset)

library(caret)
# Set seed for reproducibility
set.seed(123)

# Split the data
trainIndex <- createDataPartition(dataset$label, p = .8,
                                  list = FALSE,
                                  times = 1)
datasetTrain <- dataset[trainIndex,]
datasetTest <- dataset[-trainIndex,]

# Check the dimensions
dim(datasetTrain)
dim(datasetTest)

# Fit logistic regression model
model <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
               data = datasetTrain,
               method = "glm",
               family = "binomial")

# Check the model summary
summary(model)

# Make predictions
predictions <- predict(model, newdata = datasetTest)

# Confusion matrix
confusionMatrix(predictions, datasetTest$label)

#the fit above is error

# Check for missing values in the dataset
sapply(dataset, function(x) sum(is.na(x)))

# Remove rows with missing values
dataset <- na.omit(dataset)

# Check the dimensions again
dim(dataset)

# Split the data again after removing missing values
trainIndex <- createDataPartition(dataset$label, p = .8,
                                  list = FALSE,
                                  times = 1)
datasetTrain <- dataset[trainIndex,]
datasetTest <- dataset[-trainIndex,]

# Check the dimensions
dim(datasetTrain)
dim(datasetTest)

# Fit logistic regression model
model <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
               data = datasetTrain,
               method = "glm",
               family = "binomial")

#error stopping occur here

# Display warnings
warnings()

# Check for missing values in the training dataset
sapply(datasetTrain, function(x) sum(is.na(x)))

# Check data types
str(datasetTrain)

# Save the cleaned data
write.csv(dataset, "cleaned_FMS2.csv", row.names = FALSE)

#identify Inf Value
# Identify rows with Inf values in the speed column
inf_rows <- which(is.infinite(datasetTrain$speed))
length(inf_rows)  # Check how many rows contain Inf values

# Replace Inf values with a large finite number, for example, the max finite speed value
max_finite_speed <- max(datasetTrain$speed[!is.infinite(datasetTrain$speed)])
datasetTrain$speed[is.infinite(datasetTrain$speed)] <- max_finite_speed
datasetTest$speed[is.infinite(datasetTest$speed)] <- max_finite_speed

head(dataset)
write.csv(dataset, "cleaned_FMS2_replaceInf.csv", row.names = FALSE)

# Fit logistic regression model
model <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
               data = datasetTrain,
               method = "glm",
               family = "binomial")

# Check the model summary
summary(model)

# Make predictions
predictions <- predict(model, newdata = datasetTest)

# Confusion matrix
confusionMatrix(predictions, datasetTest$label)

# Fit decision tree model
model_tree <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
                    data = datasetTrain,
                    method = "rpart")
# Check the model summary
summary(model_tree)

#Improvement
#Hyperparameter Tuning
# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define the control using a random forest selection function
control <- trainControl(method="cv", number=10)

# Train the model with hyperparameter tuning
model_tuned <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
                     data=datasetTrain,
                     method="rpart",
                     trControl=control,
                     tuneLength=10)
# Print the best tuning parameters
print(model_tuned$bestTune)

# Make predictions on the test set
predictions_tuned <- predict(model_tuned, newdata=datasetTest)

# Evaluate the model
confusionMatrix(predictions_tuned, datasetTest$label)

#Random Forest Model
library(caret)
set.seed(123)

# Define the control using a random forest selection function
control <- trainControl(method="cv", number=10)

# Train the Random Forest model with hyperparameter tuning
model_rf <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
                  data=datasetTrain,
                  method="rf",
                  trControl=control,
                  tuneLength=10)

# Print the best tuning parameters
print(model_rf$bestTune)

# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata=datasetTest)

# Evaluate the model
confusionMatrix(predictions_rf, datasetTest$label)

#Gradient Boosting Model
library(caret)
# Set seed for reproducibility
set.seed(123)
# Define the control using a random forest selection function
control <- trainControl(method="cv", number=10)

# Train the Gradient Boosting model with hyperparameter tuning
model_gbm <- train(label ~ x_pos + y_pos + z_pos + p_pos + altitude + azimuth + speed,
                   data=datasetTrain,
                   method="gbm",
                   trControl=control,
                   tuneLength=10,
                   verbose=FALSE)
# Print the best tuning parameters
print(model_gbm$bestTune)
# Make predictions on the test set
predictions_gbm <- predict(model_gbm, newdata=datasetTest)

# Evaluate the model
conf_matrix_gbm <- confusionMatrix(predictions_gbm, datasetTest$label)
print(conf_matrix_gbm)

conf_matrix_rf <- confusionMatrix(predictions_rf, datasetTest$label)

#Comparison Model Result
# Collect model accuracies
model_accuracies <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "Gradient Boosting"),
  Accuracy = c(0.6311, 0.7533, conf_matrix_rf$overall['Accuracy'], conf_matrix_gbm$overall['Accuracy'])
)

library(ggplot2)
ggplot(model_accuracies, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  ggtitle("Model Comparison") +
  ylab("Accuracy") +
  theme_minimal()

#Feature Important for Random Forest
# Get feature importance from the random forest model
importance <- varImp(model_rf, scale = FALSE)
print(importance)

#Save model
saveRDS(model, "logistic_regression_model.rds")
saveRDS(model_tuned, "tree_model.rds")
saveRDS(model_rf, "random_forest_model.rds")
saveRDS(model_gbm, "gradient_boosting_model.rds")

conf_matrix_lr <- confusionMatrix(predictions, datasetTest$label)
conf_matrix_dt <- confusionMatrix(predictions_tuned, datasetTest$label)
  
#Metric Comparison
# Collect more metrics
model_metrics <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "Gradient Boosting"),
  Accuracy = c(conf_matrix_lr$overall['Accuracy'],
               conf_matrix_dt$overall['Accuracy'],
               conf_matrix_rf$overall['Accuracy'],
               conf_matrix_gbm$overall['Accuracy']),
  Kappa = c(conf_matrix_lr$overall['Kappa'],
            conf_matrix_dt$overall['Kappa'],
            conf_matrix_rf$overall['Kappa'],
            conf_matrix_gbm$overall['Kappa']),
  Sensitivity = c(conf_matrix_lr$byClass['Sensitivity'],
                  conf_matrix_dt$byClass['Sensitivity'],
                  conf_matrix_rf$byClass['Sensitivity'],
                  conf_matrix_gbm$byClass['Sensitivity']),
  Specificity = c(conf_matrix_lr$byClass['Specificity'],
                  conf_matrix_dt$byClass['Specificity'],
                  conf_matrix_rf$byClass['Specificity'],
                  conf_matrix_gbm$byClass['Specificity'])
)

# Print the metrics
print(model_metrics)

#Advance Model

#XGBoost
install.packages("xgboost")
library(xgboost)
library(caret)

# Check data types of training and test sets
str(datasetTrain)
str(datasetTest)

# Function to convert factors to numeric
convert_factors_to_numeric <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) {
      as.numeric(as.character(x))
    } else if (is.character(x)) {
      as.numeric(x)
    } else {
      x
    }
  })
  return(df)
}

# Convert factors and characters to numeric
datasetTrain <- convert_factors_to_numeric(datasetTrain)
datasetTest <- convert_factors_to_numeric(datasetTest)

# Remove any additional attributes
datasetTrain <- data.frame(lapply(datasetTrain, function(x) as.numeric(as.character(x))))
datasetTest <- data.frame(lapply(datasetTest, function(x) as.numeric(as.character(x))))

# Verify the conversion
str(datasetTrain)
str(datasetTest)

# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(datasetTrain[, -which(names(datasetTrain) == "label")]),
                            label = as.numeric(as.character(datasetTrain$label)))
test_matrix <- xgb.DMatrix(data = as.matrix(datasetTest[, -which(names(datasetTest) == "label")]),
                           label = as.numeric(as.character(datasetTest$label)))


# Verify the data
str(datasetTrain)
str(datasetTest)

# Set parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.3,
  max_depth = 6
)

# Train model
xgb_model <- xgboost(params = params, data = train_matrix, nrounds = 100, verbose = 0)

# Make predictions
preds_xgb <- predict(xgb_model, test_matrix)
pred_labels_xgb <- ifelse(preds_xgb > 0.5, 1, 0)

# Convert to factors with the same levels
pred_labels_xgb <- factor(pred_labels_xgb, levels = c(0, 1))
datasetTest$label <- factor(datasetTest$label, levels = c(0, 1))

# Evaluate the model
conf_matrix_xgb <- confusionMatrix(pred_labels_xgb, datasetTest$label)
print(conf_matrix_xgb)

#LightGBM
install.packages("lightgbm")
library(lightgbm)

# Prepare data
train_matrix <- lgb.Dataset(data = as.matrix(datasetTrain[, -which(names(datasetTrain) == "label")]),
                            label = as.numeric(as.character(datasetTrain$label)))
test_matrix <- as.matrix(datasetTest[, -which(names(datasetTest) == "label")])

# Set parameters
params <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.1,
  num_leaves = 31
)

# Train model
lgb_model <- lgb.train(params, train_matrix, 100)

# Make predictions
preds_lgb <- predict(lgb_model, test_matrix)
pred_labels_lgb <- ifelse(preds_lgb > 0.5, 1, 0)

# Convert to factors with the same levels
pred_labels_lgb <- factor(pred_labels_lgb, levels = c(0, 1))
datasetTest$label <- factor(datasetTest$label, levels = c(0, 1))

# Evaluate the model
conf_matrix_lgb <- confusionMatrix(pred_labels_lgb, datasetTest$label)
print(conf_matrix_lgb)

#Comparison Model Result
# Collect model accuracies
model_accuracies <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "Gradient Boosting", 
            "XBoost", "LightGBM"),
  Accuracy = c(0.6311, 0.7533, conf_matrix_rf$overall['Accuracy'], 
               conf_matrix_gbm$overall['Accuracy'], conf_matrix_xgb$overall['Accuracy'], 
               conf_matrix_lgb$overall['Accuracy'])
)
library(ggplot2)
ggplot(model_accuracies, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  ggtitle("Model Comparison") +
  ylab("Accuracy") +
  theme_minimal()

#Metric Comparison
# Collect more metrics
model_metrics <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "Gradient Boosting", "XBoost", "LightGBM"),
  Accuracy = c(conf_matrix_lr$overall['Accuracy'],
               conf_matrix_dt$overall['Accuracy'],
               conf_matrix_rf$overall['Accuracy'],
               conf_matrix_gbm$overall['Accuracy'],
               conf_matrix_xgb$overall['Accuracy'],
               conf_matrix_lgb$overall['Accuracy']),
  Kappa = c(conf_matrix_lr$overall['Kappa'],
            conf_matrix_dt$overall['Kappa'],
            conf_matrix_rf$overall['Kappa'],
            conf_matrix_gbm$overall['Kappa'],
            conf_matrix_xgb$overall['Kappa'],
            conf_matrix_lgb$overall['Kappa']),
  Sensitivity = c(conf_matrix_lr$byClass['Sensitivity'],
                  conf_matrix_dt$byClass['Sensitivity'],
                  conf_matrix_rf$byClass['Sensitivity'],
                  conf_matrix_gbm$byClass['Sensitivity'],
                  conf_matrix_xgb$overall['Sensitivity'],
                  conf_matrix_lgb$overall['Sensitivity']),
  Specificity = c(conf_matrix_lr$byClass['Specificity'],
                  conf_matrix_dt$byClass['Specificity'],
                  conf_matrix_rf$byClass['Specificity'],
                  conf_matrix_gbm$byClass['Specificity'],
                  conf_matrix_xgb$overall['Specificity'],
                  conf_matrix_lgb$overall['Specificity'])
)

# Print the metrics
print(model_metrics)






