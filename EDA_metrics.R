library(dslabs)
library(tidyverse)
library(dplyr)
library(car)
library(ggplot2)
library(ggcorrplot)
library(corrplot)

data <- as.data.frame(read.csv("imputation/imputed_data.csv"))
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
ggplot(hat.df, aes(x=hat.x))+ 
  geom_histogram(binwidth = 0.001) +
  scale_x_log10() +
  geom_vline(xintercept=cutoff, linetype="dashed", 
             color = "red", size=0.3) + 
  xlab("Log scale leverage") 


# how many to drop?
length(hat.df$hat.x[hat.df$hat.x > cutoff])  # 1564
# drop
indices <- which(hat.df$hat.x > cutoff)
target <- target[-indices]
data_new <- as.data.frame(data[-indices, ])

########################
# Correlation heatmap
########################
# cor_matrix = as.data.frame(cor(data_new))
data_new <- data_new[, !names(data_new) %in% c("FLAG_MOBIL", "FLAG_CONT_MOBILE","FLAG_DOCUMENT_21")]
M <- cor(data_new)
# head(round(M,3))
corrplot(M, method="color", tl.cex = 0.3, tl.col="black",
         col=colorRampPalette(c("darkblue","white","#C71A14"))(100))



########################
# Factor
########################
mod5 <- factanal(data_new, factors=5)
lort=loadings(mod5)[,1:5]
p = dim(data_new)[2]
heat_plot_factor <- data.frame(row=rep(1:p, times=5), 
                               col=rep(x=c('1', '2', '3', '4', '5'), each=p), 
                               value=matrix(lort))

ggplot(data=heat_plot_factor, aes(x=col, y=rev(row), fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#E68502", mid="white", high = "#E61302") +
  theme_bw() +
  scale_y_discrete(limits=rev(colnames(data_new)[1:p])) +
  ylab("") + xlab("Factors")
########################
# POST-STEP for modeling
########################

################### Calculate VIF scores for each feature ######################
# Note: this step should execute AFTER imputation
# cor_matrix = as.data.frame(cor(data, use='pairwise.complete.obs'))
# FLAG_DOCUMENT_2
# ggcorrplot(cor_matrix)

model <- lm(TARGET ~ ., data = data)
vif_scores <- vif(model)
high_vif_ftrs <- vif_scores[vif_scores > 5]
# drop high vif columns
data <- data[, !names(data) %in% (names(high_vif_ftrs))]
dim(data)

# draw a correlation plot
