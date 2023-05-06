library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(magrittr)

#Loading data
data <- as.data.frame(read.csv("/application_train_preprocessed.csv"))

################# PREPROCESSING ####################
#Extracting the TARGET
target <- data$TARGET

#remove TARGET column from the dataset for preprocessing
data <- data[, -which(names(data) == "TARGET")]

#select only numeric columns
nume_train = data[, unlist(lapply(data, is.numeric))]
#removing 3 columns that are almost collinear
nume_train<- nume_train[, !names(nume_train) %in% c("FLAG_MOBIL", "FLAG_CONT_MOBILE",
                                               "FLAG_DOCUMENT_21")]
# remove missing data from X and TARGET
nume_train_nna <- na.omit(nume_train)
removed_indices <- setdiff(seq_len(nrow(nume_train)), seq_len(nrow(nume_train_nna)))
target_nna <- target[-removed_indices]

#scale data
scaled_data <- as.data.frame(scale(nume_train_nna))

################# Logistic regression modeling ####################
#combining data for training
model_data <- scaled_data
model_data$TARGET <- target_nna

#Splitting for train and test (80-20%)
train_lm = model_data %>% sample_frac(0.8)
train_lm <- train_lm[complete.cases(train_lm),]

val = model_data %>% setdiff(train_lm)
val_x = val[, -which(names(val) == "TARGET")]
val_target = val$TARGET

#logistic Regression Model
loglm <- glm(TARGET ~ ., data=train_lm, family=binomial(link = 'logit'))

#Predicting on Val data 
predictions <- predict(loglm, newdata = val_x, type = "response")

# Convert the probabilities to class labels (0 or 1)
predicted_class <- as.factor(ifelse(predictions > 0.5, 1, 0))
actual_class <- as.factor(val_target)

#Getting the Confusion Matrix, ROC and AUROC
conf_matrix <- confusionMatrix(predicted_class, actual_class)
roc.curve(actual_class, predicted_class)









