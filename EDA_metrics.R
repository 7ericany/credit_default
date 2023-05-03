library(dslabs)
library(tidyverse)
library(dplyr)
library(car)
library(ggplot2)
library(ggcorrplot)

data <- as.data.frame(read.csv("data_cleaned/train_encode_cleaned.csv"))
data <- data[3:56] # get rid of the ID and fake_id columns

################# Calculate leverage scores for each sample ####################



### PRE-STEP for modeling

################### Calculate VIF scores for each feature ######################
# Note: this step should execute AFTER imputation
# Drop FLAG_MOBIL
data <- data[, -which(names(data) == "FLAG_MOBIL")]
cor_matrix = as.data.frame(cor(data, use='pairwise.complete.obs'))
# FLAG_DOCUMENT_2
# ggcorrplot(cor_matrix)

model <- lm(TARGET ~ ., data = data)
vif_scores <- vif(model)
high_vif_ftrs <- vif_scores[vif_scores > 5]
# drop high vif columns
data <- data[, !names(data) %in% (names(high_vif_ftrs))]
dim(data)

# draw a correlation plot
