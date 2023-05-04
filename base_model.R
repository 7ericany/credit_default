library(dslabs)
library(tidyverse)
library(dplyr)
#library(car)
library(ggplot2)
library(caret)
library(magrittr)

data <- as.data.frame(read.csv("/Users/radhikamehrotra/Documents/DATA2020/credit_fraud/preprocessed/application_train_preprocessed.csv"))
target <- data$TARGET


# remove TARGET column
data <- data[, -which(names(data) == "TARGET")]


#select only numeric columns
nume_train = data[, unlist(lapply(data, is.numeric))]
nume_train<- nume_train[, !names(nume_train) %in% c("FLAG_MOBIL", "FLAG_CONT_MOBILE",
                                               "FLAG_DOCUMENT_21")]
# remove missing
nume_train_nna <- na.omit(nume_train)

removed_indices <- setdiff(seq_len(nrow(nume_train)), seq_len(nrow(nume_train_nna)))



# Remove same rows from another column
target_nna <- target[-removed_indices]



#scale data
scaled_data <- as.data.frame(scale(nume_train_nna))



################# Logistic regression modeling ####################
model_data <- scaled_data
model_data$TARGET <- target_nna



train_lm = model_data %>% sample_frac(0.8)
#train_lm <- na.omit(train_lm)
train_lm <- train_lm[complete.cases(train_lm),]
#print(train_lm)
val = model_data %>% setdiff(train_lm)
val_x = val[, -which(names(val) == "TARGET")]
val_target = val$TARGET


loglm <- glm(TARGET ~ ., data=train_lm, family=binomial(link = 'logit'))

# Print a summary of the logistic regression model
#summary(loglm)

predictions <- predict(loglm, newdata = val_x, type = "response")

# Convert the probabilities to class labels (0 or 1)
predicted_class <- as.factor(ifelse(predictions > 0.5, 1, 0))
actual_class <- as.factor(val_target)

conf_matrix <- confusionMatrix(predicted_class, actual_class)
conf_matrix
# extract precision and recall from the confusion matrix
#precision <- conf_matrix$byClass["Pos Pred Value"]
#recall <- conf_matrix$byClass["Sensitivity"]
#precision
#recall
#predicted_classes <- factor(predicted_class, levels = levels(actual_class))

#error rate metric 
#error_rate <- mean(predicted_class != actual_class)
error_rate

roc.curve(actual_class, predicted_class)









