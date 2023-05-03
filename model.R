library(dslabs)
library(tidyverse)
library(dplyr)
#library(car)
library(ggplot2)
library(caret)

data <- as.data.frame(read.csv("/Users/radhikamehrotra/Documents/DATA2020/credit_fraud/imputation/imputed_data.csv"))
target <- data$TARGET
# remove TARGET column
data <- data[, -which(names(data) == "TARGET")]
data <- scale(data)
# data <- data[3:56] # get rid of the ID and fake_id columns

################# Calculate leverage scores for each sample ####################
# ISLRv2 p99 (formula 3.37)
middle_point <- colMeans(data)
euc_dist2 <- function(x) {sum((x - middle_point)^2)}
distances <- apply(data, 1, euc_dist2)
all_sum <- sum(distances)
hat.x <- (1 / length(distances)) + distances / all_sum
hat.df <- as.data.frame(hat.x)
cutoff = 2 * 1e-5
# ggplot(hat.df, aes(x=hat.x))+ 
#   geom_histogram(binwidth = 0.001) +
#   scale_x_log10() +
#   geom_vline(xintercept=cutoff, linetype="dashed", 
#              color = "red", size=0.3) + 
#   xlab("Log scale leverage") 

# how many to drop?
length(hat.df$hat.x[hat.df$hat.x > cutoff])
# drop
indices <- which(hat.df$hat.x > cutoff)
target <- target[-indices]
data <- as.data.frame(data[-indices, ])

################# Logistic regression modeling ####################

# define the indices for stratified K-fold cross-validation
model_data <- data
model_data$TARGET <- target
folds <- createFolds(target, k = 2, list = TRUE, 
                     returnTrain = TRUE)

# train and evaluate the model using stratified K-fold cross-validation
for (i in 1:length(folds)) {
  train_data <- model_data[folds[[i]], ]
  test_data <- model_data[-folds[[i]], ]
  
  
  
  # your model training code goes here
  # for example:
  model <- glm(TARGET ~ ., data = train_data, family = "binomial")
  
  # your model evaluation code goes here
  # for example:
  predicted <- predict(model, newdata = test_data, type = "response")
  threshold <- 0.5
  predicted_class <- as.factor(ifelse(predicted > threshold, 1, 0))
  #print(class(predicted_class))
  actual_class <- as.factor(test_data$TARGET)
  #print(class(actual_class))
  
  #We have to choose a different metric (TP/FP/TN/FN)
  conf_matrix <- confusionMatrix(predicted_class, actual_class)
  
  # extract precision and recall from the confusion matrix 
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  
  print(paste0("Fold ", i, " Precision: ", precision, "Recall", recall))
}