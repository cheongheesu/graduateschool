data <- read.csv("FMS2.csv")
head(data)
tail(data)

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Histograms for each feature using base R
par(mfrow=c(3,2))  # Setting the plot area to contain 3 rows and 2 columns
for (feature in c("x_pos", "y_pos", "z_pos", "p_pos", "altitude", "azimuth")) {
  hist(data[[feature]], main=paste(feature, "Distribution"), xlab=feature, col='blue', breaks=30)
}

library(ggplot2)

# Create a function to plot histograms with ggplot2
plot_histogram <- function(data, feature) {
  ggplot(data, aes_string(x=feature)) +
    geom_histogram(bins=30, fill="steelblue", color="black") +
    labs(title=paste(feature, "Distribution"), x=feature, y="Frequency")
}

# Plotting histograms for each feature
feature_names <- c("x_pos", "y_pos", "z_pos", "p_pos", "altitude", "azimuth")
par(mfrow=c(3,2))  # Arrange plots in 3 rows and 2 columns
for (feature in feature_names) {
  print(plot_histogram(data, feature))
}
library(dplyr)
library(lubridate)

# Convert log_time from MM:SS.F to total seconds
data <- data %>%
  mutate(
    minutes = as.numeric(sub(":.+$", "", log_time)),  # Extract minutes part
    seconds = as.numeric(sub("^[^:]+:", "", log_time)),  # Extract seconds part including fractional
    total_seconds = minutes * 60 + seconds  # Convert to total seconds
  ) %>%
  select(-minutes, -seconds)  # Optional: remove the intermediate columns

# Check the transformed data
print(head(data$total_seconds))

# Calculate time differences
data <- data %>% mutate(time_diff = c(NA, diff(total_seconds)))

# Calculate velocities and accelerations based on x_pos and y_pos
data <- data %>% mutate(
  x_diff = c(NA, diff(x_pos)),
  y_diff = c(NA, diff(y_pos)),
  velocity = sqrt(x_diff^2 + y_diff^2) / time_diff,
  acceleration = c(NA, diff(velocity)) / time_diff
)

ggplot(data, aes(x = total_seconds, y = velocity)) +
  geom_line() +
  labs(title = "Velocity Over Time", x = "Time (seconds)", y = "Velocity")

# Check rows with high velocity for any data anomalies
data %>%
  arrange(desc(velocity)) %>%
  head(20)

# Applying a simple filter to remove unreasonably high velocities
reasonable_velocity <- data %>%
  filter(velocity < 200)  # Adjust this threshold based on typical handwriting speeds observed

# Replot with filtered data
ggplot(reasonable_velocity, aes(x = total_seconds, y = velocity)) +
  geom_line() +
  labs(title = "Filtered Velocity Over Time", x = "Time (seconds)", y = "Velocity")

# Adjust time differences where they are zero (replace zero with a small number, e.g., 0.001)
data <- data %>%
  mutate(
    time_diff = ifelse(time_diff == 0, 0.001, time_diff),  # Replace 0 with a small number
    velocity = sqrt(x_diff^2 + y_diff^2) / time_diff,
    acceleration = c(NA, diff(velocity) / time_diff)
  )

# Optional: filter out any remaining unrealistic velocities if they are still present
data <- data %>%
  filter(velocity < 1000)  # You might need to adjust this threshold based on expected velocities

# Replot the velocity over time to check the corrections
library(ggplot2)
ggplot(data, aes(x = total_seconds, y = velocity)) +
  geom_line() +
  labs(title = "Corrected Velocity Over Time", x = "Time (seconds)", y = "Velocity")

library(zoo)

# Applying a moving average for smoothing the velocity
data <- data %>%
  mutate(velocity_smooth = rollmean(velocity, 5, fill = NA, align = "center"))  # 5-point moving average

# Plot the smoothed velocity over time
ggplot(data, aes(x = total_seconds, y = velocity_smooth)) +
  geom_line() +
  ylim(0, 200) +  # Adjust this limit based on what you find reasonable
  labs(title = "Smoothed Velocity Over Time", x = "Time (seconds)", y = "Smoothed Velocity")

#Deeper Smoothing
# Applying a stronger smoothing using a larger window for the moving average
data$velocity_smooth_stronger <- rollmean(data$velocity, k = 15, fill = NA, align = "center")  # 15-point moving average

# Plot the more strongly smoothed velocity
ggplot(data, aes(x = total_seconds, y = velocity_smooth_stronger)) +
  geom_line() +
  ylim(0, 200) +  # Adjust if necessary
  labs(title = "More Strongly Smoothed Velocity Over Time", x = "Time (seconds)", y = "Smoothed Velocity")

# Identifying segments with high velocity
high_velocity_segments <- data %>%
  filter(velocity_smooth > 100)  # Adjust the threshold based on what you consider "high velocity"

# Analyze these segments to see what's happening during these times
print(head(high_velocity_segments))

#Adjust Calculation for Acceleration and Velocity
# Adjusting acceleration calculation
data <- data %>%
  mutate(
    acceleration = ifelse(time_diff == 0, NA, diff(velocity, lag = 1, differences = 1) / time_diff)
  )
# Further filtering extreme velocities
data <- data %>%
  filter(velocity < 200)  # Adjust the threshold based on further insights
# Recalculate smoothed velocity
data$velocity_smooth_stronger <- rollmean(data$velocity, k = 15, fill = NA, align = "center")
# Replotting
ggplot(data, aes(x = total_seconds, y = velocity_smooth_stronger)) +
  geom_line() +
  labs(title = "Refined Smoothed Velocity Over Time", x = "Time (seconds)", y = "Smoothed Velocity")

# Examining segments with very high velocities
high_velocity_data <- data %>%
  filter(velocity > 100) %>%
  arrange(desc(velocity))
print(high_velocity_data)

# Applying a more robust smoothing technique, like LOESS
ggplot(data, aes(x = total_seconds, y = velocity)) +
  geom_smooth(method = "loess", span = 0.05) +  # LOESS smoothing
  labs(title = "LOESS Smoothed Velocity Over Time", x = "Time (seconds)", y = "Velocity")

library(dplyr)
library(zoo)
library(caret)

# Calculate more robust features for velocity and acceleration
data$velocity_smooth = rollmean(data$velocity, k = 5, fill = NA, align = "center")  # Smoothed velocity
data$acceleration_smooth = rollmean(data$acceleration, k = 5, fill = NA, align = "center")  # Smoothed acceleration

# Recalculate features including p_pos
features <- data %>%
  group_by(person) %>%
  summarize(
    avg_velocity = mean(velocity_smooth, na.rm = TRUE),
    sd_velocity = sd(velocity_smooth, na.rm = TRUE),
    avg_acceleration = mean(acceleration_smooth, na.rm = TRUE),
    sd_acceleration = sd(acceleration_smooth, na.rm = TRUE),
    max_acceleration = max(abs(acceleration_smooth), na.rm = TRUE),
    avg_pressure = mean(p_pos, na.rm = TRUE),  # Average pressure
    sd_pressure = sd(p_pos, na.rm = TRUE)  	# Standard deviation of pressure
  )
# Check summary statistics of pressure data
summary(features$avg_pressure)

hist(features$avg_pressure, main = "Distribution of Average Pressure", xlab = "Average Pressure", breaks = 20)

# Artificially adjusting thresholds for demonstration
features$skill_level <- ifelse(
  features$avg_pressure > 250 & features$max_acceleration < 500,
  "Good",
  "Bad"
)

# Re-check the class distribution
table(features$skill_level)

# T-test for average pressure between groups
t_test_result <- t.test(features$avg_pressure ~ features$skill_level)
print(t_test_result)

# ANOVA for overall effect of skill level on pressure and acceleration
anova_result <- aov(max_acceleration ~ skill_level, data = features)
summary(anova_result)

# Plotting with new classifications
ggplot(features, aes(x = avg_pressure, y = max_acceleration, color = skill_level)) +
  geom_point() +
  labs(title = "Adjusted Pressure vs. Acceleration by Skill Level", x = "Average Pressure", y = "Maximum Acceleration")

library(ggplot2)

# Box plot for average pressure by skill level
ggplot(features, aes(x = skill_level, y = avg_pressure, fill = skill_level)) +
  geom_boxplot() +
  labs(title = "Box Plot of Average Pressure by Skill Level", x = "Skill Level", y = "Average Pressure")

# Box plot for maximum acceleration by skill level
ggplot(features, aes(x = skill_level, y = max_acceleration, fill = skill_level)) +
  geom_boxplot() +
  labs(title = "Box Plot of Maximum Acceleration by Skill Level", x = "Skill Level", y = "Maximum Acceleration")

library(caret)
set.seed(123)  # for reproducibility

# Use stratified sampling to ensure balance
training_indices <- createDataPartition(features$skill_level, p = 0.8, list = TRUE, times = 1)

# Check class distribution in the training set
table(train_data$skill_level)
# Check class distribution in the testing set
table(test_data$skill_level)

model <- train(
  skill_level ~ avg_pressure + max_acceleration,
  data = train_data,
  method = "rf",
  metric = "ROC",
  trControl = train_control
)

# Print the model summary to see how it performed
print(model)

# Predict on the test data
predictions <- predict(model, test_data, type = "prob")
# Assuming the positive class is 'Good'
good_probabilities <- predictions[, "Good"]

print(predictions)
print(good_probabilities)



#use ROSE to add more sample data
#handling the imbalance dataset
install.packages("ROSE")
library(ROSE)

rose_data <- ovun.sample(skill_level ~ ., data = train_data, method = "both", N = 500, seed = 1)$data

# Check the new distribution of the classes
table(rose_data$skill_level)

#Re-train the model using balanced dataset
# Load necessary library
library(randomForest)

# Train the model
model <- randomForest(skill_level ~ ., data = rose_data, ntree = 100)

# Convert predictions to a factor with levels matching the training data
predictions <- factor(predictions, levels = levels(train_data$skill_level))

# Also ensure the test data skill level is a factor (if it's not already)
test_data$skill_level <- factor(test_data$skill_level, levels = levels(train_data$skill_level))

# Try the confusion matrix again
conf_mat <- confusionMatrix(predictions, test_data$skill_level)

# Print the confusion matrix
print(conf_mat)

print(summary(model))

# Check feature importance
importance(model)

#Comparing Method
model_rf <- randomForest(skill_level ~ ., data = rose_data, ntree = 100)

library(e1071)
model_svm <- svm(skill_level ~ ., data = rose_data, type = 'C-classification', kernel = 'radial')

install.packages("gbm")
library(gbm)
model_gbm <- gbm(skill_level ~ ., data = rose_data, distribution = "bernoulli", n.trees = 1000, interaction.depth = 1)

# Predictions
predictions_rf <- predict(model_rf, test_data)
predictions_svm <- predict(model_svm, test_data)
predictions_gbm <- predict(model_gbm, test_data, type = "response")

# Confusion Matrix and Accuracy
library(caret)
conf_mat_rf <- confusionMatrix(predictions_rf, test_data$skill_level)
conf_mat_svm <- confusionMatrix(as.factor(predictions_svm), test_data$skill_level)
conf_mat_gbm <- confusionMatrix(as.factor(ifelse(predictions_gbm > 0.5, "Good", "Bad")), test_data$skill_level)

# Print results
print(conf_mat_rf)
print(conf_mat_svm)
print(conf_mat_gbm)

model_gbm <- gbm(skill_level ~ ., data = train_data,
                 distribution = "bernoulli", # use 'bernoulli' for binary classification
                 n.trees = 50,         	# number of trees
                 interaction.depth = 1, 	# depth of tree
                 shrinkage = 0.1,      	# learning rate
                 bag.fraction = 1,  # No subsampling
                 train.fraction = 0.8,  # Use 80% of data for training in each iteration
                 n.minobsinnode = 10)  # Ensure not too small node size
# Summary of the model
summary(model_gbm)

library(caret)

# Set up cross-validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,  # for ROC later
                           savePredictions = "final")

# Train the model using caret
model_gbm_caret <- train(skill_level ~ ., data = train_data,
                         method = "gbm",
                         trControl = fitControl,
                         verbose = FALSE,
                         metric = "ROC",
                         tuneGrid = expand.grid(interaction.depth = c(1, 2),
                                                n.trees = c(50, 100),
                                                shrinkage = c(0.1, 0.05),
                                                n.minobsinnode = c(10)))

# Check results
print(model_gbm_caret)


