library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)

#Loading data 
data <- read.csv(paste("imputation/imputed_data.csv", sep=''))
target <- data$TARGET

# remove TARGET column
data <- data[, -which(names(data) == "TARGET")]
data <- scale(data)


################# Calculate leverage scores for each sample ####################
# ISLRv2 p99 (formula 3.37)
middle_point <- colMeans(data)
euc_dist2 <- function(x) {sum((x - middle_point)^2)}
distances <- apply(data, 1, euc_dist2)
all_sum <- sum(distances)
hat.x <- (1 / length(distances)) + distances / all_sum
hat.df <- as.data.frame(hat.x)
cutoff = 2 * 1e-5

# drop
indices <- which(hat.df$hat.x > cutoff)
target <- target[-indices]
data <- as.data.frame(data[-indices, ])

################# Logistic regression modeling ####################
# define the indices for stratified K-fold cross-validation

model_data <- data
model_data$TARGET <- target
folds <- createFolds(target, k = 10, list = TRUE,
                     returnTrain = TRUE)

# train and evaluate the model using stratified K-fold cross-validation
boot_roc <- list()
for (i in 1:length(folds)) {
  train_data <- model_data[folds[[i]], ]
  test_data <- model_data[-folds[[i]], ]

  model <- glm(TARGET ~ ., data = train_data, family = "binomial")

  predicted <- predict(model, newdata = test_data, type = "response")
  threshold <- 0.5
  predicted_class <- as.factor(ifelse(predicted > threshold, 1, 0))

  actual_class <- as.factor(test_data$TARGET)

  conf_matrix <- confusionMatrix(predicted_class, actual_class)
  
  roc <- roc.curve(actual_class, predicted_class)
  boot_roc <- c(boot_roc, roc) 
}

#Getting the Confusion Matrix, ROC and AUROC
conf_matrix
roc.curve










