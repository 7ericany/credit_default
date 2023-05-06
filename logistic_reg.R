
library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)

###################### model.r in github
data <- read.csv(paste("~/Downloads/DATA2020/credit_fraud/imputation/imputed_data.csv", sep=''))
target <- data$TARGET

# remove TARGET column
data <- data[, -which(names(data) == "TARGET")]
data <- scale(data)
#data <- data[, !names(data) %in% c("FLAG_MOBIL", "FLAG_CONT_MOBILE",
#                                       "FLAG_DOCUMENT_21")]
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
# folds <- createFolds(target, k = 10, list = TRUE,
#                      returnTrain = TRUE)

full.model <- glm(TARGET ~ ., data=model_data, family="binomial")
none.model <- glm(TARGET ~ 1, data=model_data, family="binomial")
step.model <- step(none.model, scope=list(upper=full.model, lower=none.model),
                   direction = "forward")
summary(step.model)

# train and evaluate the model using stratified K-fold cross-validation
boot_roc <- list()
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
  
  roc <- roc.curve(actual_class, predicted_class)
  boot_roc <- c(boot_roc, roc) 
  
  # extract precision and recall from the confusion matrix
  #precision <- conf_matrix$byClass["Pos Pred Value"]
  #recall <- conf_matrix$byClass["Sensitivity"]
  #print(paste0("Fold ", i, " Precision: ", precision, "Recall", recall))
}

boot_roc
mean(boot_roc)


# Estimate   Std. Error    z value     Pr(>|z|)
# CODE_GENDER                  4.223241e-01 2.220169e-02  19.022162 1.117781e-80
# CNT_CHILDREN                 3.572782e-01 4.909941e-02   7.276629 3.422650e-13
# REG_REGION_NOT_WORK_REGION   3.401981e-01 1.396659e-01   2.435798 1.485896e-02
# REG_CITY_NOT_LIVE_CITY       3.006189e-01 5.404721e-02   5.562154 2.664646e-08
# REGION_RATING_CLIENT_W_CITY  2.082149e-01 6.596966e-02   3.156222 1.598269e-03
# FLAG_DOCUMENT_3              1.698537e-01 2.741455e-02   6.195749 5.800842e-10
# NAME_EDUCATION_TYPE          1.475732e-01 8.332139e-03  17.711327 3.428921e-70
# FLAG_WORK_PHONE              1.444558e-01 2.627945e-02   5.496911 3.865024e-08
# DEF_60_CNT_SOCIAL_CIRCLE     1.030032e-01 4.973789e-02   2.070919 3.836636e-02
# DEF_30_CNT_SOCIAL_CIRCLE     9.172102e-02 4.147370e-02   2.211547 2.699801e-02
# NAME_INCOME_TYPE             1.841613e-02 4.138069e-03   4.450416 8.570400e-06
# AMT_REQ_CREDIT_BUREAU_YEAR   1.601807e-02 5.264213e-03   3.042823 2.343700e-03
# WEEKDAY_APPR_PROCESS_START   1.112647e-02 4.540482e-03   2.450505 1.426561e-02
# OCCUPATION_TYPE              6.508833e-03 2.243413e-03   2.901309 3.716077e-03
# DAYS_LAST_PHONE_CHANGE       8.919929e-05 1.284783e-05   6.942750 3.845389e-12
# DAYS_ID_PUBLISH              5.761771e-05 6.852898e-06   8.407787 4.178177e-17
# DAYS_BIRTH                   2.538128e-05 3.236880e-06   7.841278 4.459831e-15
# DAYS_REGISTRATION            1.262305e-05 3.047877e-06   4.141587 3.449117e-05
# AMT_ANNUITY                  6.811830e-06 1.164882e-06   5.847658 4.985409e-09
# AMT_CREDIT                  -1.532250e-07 4.111765e-08  -3.726503 1.941549e-04
# HOUR_APPR_PROCESS_START     -8.594643e-03 3.188244e-03  -2.695730 7.023463e-03
# AMT_REQ_CREDIT_BUREAU_QRT   -5.212215e-02 1.568662e-02  -3.322714 8.914615e-04
# NAME_FAMILY_STATUS          -9.606634e-02 1.742716e-02  -5.512451 3.538712e-08
# FLAG_PHONE                  -1.086969e-01 2.375921e-02  -4.574936 4.763658e-06
# FLAG_OWN_CAR                -2.664882e-01 2.276248e-02 -11.707348 1.168784e-31
# NAME_CONTRACT_TYPE          -2.901140e-01 4.420662e-02  -6.562682 5.284862e-11
# CNT_FAM_MEMBERS             -3.357596e-01 4.521674e-02  -7.425561 1.123029e-13
# LIVE_REGION_NOT_WORK_REGION -3.728626e-01 1.393255e-01  -2.676198 7.446271e-03
# REG_REGION_NOT_LIVE_REGION  -4.028219e-01 1.307137e-01  -3.081712 2.058140e-03
# EXT_SOURCE_2                -2.088455e+00 5.274371e-02 -39.596287 0.000000e+00
# EXT_SOURCE_3                -2.152924e+00 4.948651e-02 -43.505270 0.000000e+00

# CODE_GENDER                  4.223241e-01 2.220169e-02  19.022162 1.117781e-80
# FLAG_OWN_CAR                -2.664882e-01 2.276248e-02 -11.707348 1.168784e-31
# CNT_CHILDREN                 3.572782e-01 4.909941e-02   7.276629 3.422650e-13
# NAME_EDUCATION_TYPE          1.475732e-01 8.332139e-03  17.711327 3.428921e-70
# EXT_SOURCE                  -2.088455e+00 5.274371e-02 -39.596287 0.000000e+00


features <- c("CODE_GENDER", "FLAG_OWN_CAR", "CNT_CHILDREN", "NAME_EDUCATION_TYPE") #"EXT_SOURCE"
logodds <- c(4.223241e-01, -2.664882e-01, 3.572782e-01, 1.475732e-01) #, -2.088455e+00
sderr <- c(2.220169e-02, 2.276248e-02, 4.909941e-02, 8.332139e-03) #, 5.274371e-02
upper <- exp(logodds + sderr)
lower <- exp(logodds - sderr)
error_bar <- data.frame(features=features, logodds=logodds, 
                        lower=lower, upper=upper)
ggplot(data=error_bar, aes(x=features, y=estimate_odds, group=1))  +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05)) +
  # scale_x_discrete(labels=) +
  theme_bw()

# bootstrap

n_boots <- 10
betas <- matrix(NA, n_boots, 5)

for (i in 1:n_boots){
  idx_boot <- sample(c(1:dim(model_data)[1]),dim(model_data)[1], replace=T)
  data_boot <- model_data[idx_boot,] 
  model <- glm(formula = TARGET ~ ., data = data_boot, family="binomial")
  
  betas[i, 1] <- model$coefficients["CODE_GENDER"]
  betas[i, 2] <- model$coefficients["CNT_CHILDREN"]
  betas[i, 3] <- model$coefficients["REG_CITY_NOT_LIVE_CITY"]
  betas[i, 4] <- model$coefficients["FLAG_DOCUMENT_3"]
  betas[i, 5] <- model$coefficients["NAME_EDUCATION_TYPE"]
}

beta.df <- exp(as.data.frame(betas))
colnames(beta.df)[colnames(beta.df) == "V1"] ="CODE_GENDER"
colnames(beta.df)[colnames(beta.df) == "V2"] ="CNT_CHILDREN"
colnames(beta.df)[colnames(beta.df) == "V3"] ="REG_CITY_NOT_LIVE_CITY"
colnames(beta.df)[colnames(beta.df) == "V4"] ="FLAG_DOCUMENT_3"
colnames(beta.df)[colnames(beta.df) == "V5"] ="NAME_EDUCATION_TYPE"

beta.expand <- beta.df %>% pivot_longer(names_to = "Beta", 
                                        values_to = "Value", 
                                        c("CODE_GENDER", "CNT_CHILDREN", 
                                          "REG_CITY_NOT_LIVE_CITY", 
                                          "FLAG_DOCUMENT_3", 
                                          "NAME_EDUCATION_TYPE"))

ggplot(data=beta.expand, aes(x=Beta, y=Value, fill=Beta)) +
  geom_boxplot(alpha=0.5) +
  ylab("Log Odds") +
  scale_fill_manual(values=c("#D10505", "#D15805", "#0A2EA9", "#F18150", "#040B97")) + 
  theme_bw() + 
  scale_x_discrete(name ="Postive covariates", 
                   labels=c("Gender", "CNT_Children", 
                            "Region_city", 
                            "FLAG_Document", 
                            "Education")) + 
  labs(title="Bootstrap Log Odds Estimation")
