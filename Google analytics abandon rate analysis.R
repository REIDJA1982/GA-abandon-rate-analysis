## I will generate synthetic google analytics data for abandonment analysis

# get some tools

library(glmnet)
library(ggplot2)

# Generate random data
set.seed(123)
n <- 10000

# Assign device type
device_type <- c("desktop", "tablet", "smartphone")
visitors <- data.frame(id=1:n, 
                       start_time=as.POSIXct("2022-01-01") + runif(n)*60*60*24*30, 
                       end_time=as.POSIXct(NA), 
                       abandoned=NA,
                       device=sample(device_type, n, replace = TRUE))

# Assign age
visitors$age <- floor(runif(n, 18, 66))

# Assign end times and abandoned status
for(i in 1:n) {
  prob <- 0.2 # Default abandonment rate
  if(visitors[i, "age"] > 40) prob <- prob + 0.05 # Increase abandonment rate for older clients
  if(visitors[i, "device"] == "smartphone") prob <- prob + 0.4 # Increase abandonment rate for smartphone users
  if(runif(1) > prob) {
    visitors[i, "end_time"] <- visitors[i, "start_time"] + runif(1)*3600 + rnorm(1,0,600) # Add random error to end time 
    visitors[i, "abandoned"] <- rbinom(1,1,0.1) # Add random variation to abandoned status
  } else {
    visitors[i, "end_time"] <- as.POSIXct(NA)
    visitors[i, "abandoned"] <- 1
  }
}


View(visitors)

# Filter out abandoned sessions
visitors_completed <- visitors[visitors$abandoned == 0,]

# Calculate duration and abandonment rate
visitors_completed$duration <- as.numeric(difftime(visitors_completed$end_time, visitors_completed$start_time, unit='sec'))
abandonment_rate <- sum(visitors$abandoned) / n

# calculate abandonment rate by age
abandonment_rate_by_age <- aggregate(visitors$abandoned, by=list(age=visitors$age), mean)
names(abandonment_rate_by_age) <- c("age", "abandonment_rate")

# calculate abandonment rate by device
abandonment_rate_by_device <- aggregate(visitors$abandoned, by=list(device=visitors$device), mean)
names(abandonment_rate_by_device) <- c("device", "abandonment_rate")

# plot abandonment rate by device

device <- ggplot(abandonment_rate_by_device, aes(x=device, y=abandonment_rate)) +
  geom_bar(stat="identity") +
  ggtitle("Abandonment rate by Device") +
  xlab("Device") +
  ylab("Abandonment rate")
print(device)

# plot abandonment rate by age
ggplot(abandonment_rate_by_age, aes(x=age, y=abandonment_rate)) +
  geom_line() +
  ggtitle("Abandonment rate by Age") +
  xlab("Age") +
  ylab("Abandonment rate")


# convert device variable to a factor
visitors$device <- as.factor(visitors$device)

# Split data into training and testing sets
set.seed(1234)
train_index <- sample(1:n, 0.8*n)
train_data <- visitors[train_index, ]
test_data <- visitors[-train_index, ]

# Fit a logistic regression model
fit <- glm(abandoned ~ age + device, data = train_data, family = "binomial")

# Make predictions on the test data
predictions <- predict(fit, test_data, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)
test_data$predictions <- predictions

View(test_data)


# Calculate the accuracy of the model
accuracy <- mean(predictions == test_data$abandoned)


# Create a confusion matrix
train_confusion_matrix <- table(predicted = predictions, actual = test_data$abandoned)

# Calculate precision, recall and f1-score
train_precision <- train_confusion_matrix[2,2] / sum(train_confusion_matrix[2,])
train_recall <- train_confusion_matrix[2,2] / sum(train_confusion_matrix[,2])
train_f1 <- 2 * (train_precision * train_recall) / (train_precision + train_recall)

# Calculate ROC-AUC
library(ROCR)
predictions_probs <- predict(fit,train_data,type="response")
train_pred <- prediction(predictions_probs, train_data$abandoned)
train_perf <- performance(train_pred, "auc")
train_auc <- train_perf@y.values[[1]]

# Print the results
cat("Accuracy:", train_accuracy)
cat("\nConfusion Matrix:\n", train_confusion_matrix)
cat("\nPrecision:", train_precision)
cat("\nRecall:", train_recall)
cat("\nF1-Score:", train_f1)
cat("\nROC-AUC:", train_auc)

rm(list = ls())
