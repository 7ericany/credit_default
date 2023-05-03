library(dslabs)
library(tidyverse)
library(dplyr)
library(car)
library(ggplot2)
library(ggcorrplot)

data <- as.data.frame(read.csv("imputation/imputed_data.csv"))
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
ggplot(hat.df, aes(x=hat.x))+ 
  geom_histogram(binwidth = 0.001) +
  scale_x_log10() +
  geom_vline(xintercept=3 * 1e-5, linetype="dashed", 
             color = "red", size=0.3) + 
  xlab("Log scale leverage") 

cutoff = 3 * 1e-5
# how many to drop?
length(hat.df$hat.x[hat.df$hat.x > cutoff])
# 

########################
# POST-STEP for modeling
########################







################### Calculate VIF scores for each feature ######################
# Note: this step should execute AFTER imputation
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
