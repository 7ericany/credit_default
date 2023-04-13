library(ggplot2)
library(reshape2)
library(dslabs)
library(tidyverse)
library(dplyr)
library(ggcorrplot)

##################### Load training data ########################
data_folder = "~/Downloads/home-credit-default-risk/"
train = as.data.frame(read.csv(paste(data_folder, "application_train.csv", sep='')))
# convert to data frame
# train <- do.call(rbind.data.frame, train)
class(train)

##################### Preprocessing ########################
# show number of missing values in each col
train %>% summarise_all(~ sum(is.na(.)))
sapply(train, class)

##################### basic basic model ########################
# just to get a sense
# non-missing, integer/numeric
library(caret)
nume_train = train[, unlist(lapply(train, is.numeric))]
nume_train

# remove missing
nume_train_nna <- na.omit(nume_train)
# drop FLAG_DOCUMENT_2 
nume_train_nna <- nume_train_nna[, -which(names(nume_train_nna) == "FLAG_DOCUMENT_2")]
nume_train_nna <- nume_train_nna[, -which(names(nume_train_nna) == "FLAG_MOBIL")]

train_lm = nume_train_nna %>% sample_frac(0.8)
val = nume_train_nna %>% setdiff(train_lm)

loglm <- glm(data=train_lm, TARGET ~ ., family="binomial")
my_model <- train(TARGET ~. ,data=nume_train_nna, method="glm",
                  trControl=trainControl(method="cv"),
                  family="binomial")
summary(my_model)
# backward

################### EDA #####################
cor_matrix = as.data.frame(cor(nume_train_nna))
# FLAG_DOCUMENT_2
ggcorrplot(cor_matrix)

# we will need a feature selection backward/forward method
# correlation-based feature selection (CFS), method=kendall, MRMR, relief()
# option: PCA
# lasso/ridge
