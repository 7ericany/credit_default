library(ggplot2)
library(reshape2)
library(dslabs)
library(tidyverse)
library(dplyr)
library(ggcorrplot)

##################### Load training data ########################
data_folder = "./preprocessed/"
train = as.data.frame(read.csv(paste(data_folder, "application_train_preprocessed.csv", sep='')))
# convert to data frame
# train <- do.call(rbind.data.frame, train)
class(train)

##################### Preprocessing ########################
# show number of missing values in each col
train %>% summarise_all(~ sum(is.na(.)))
sapply(train, class)
## 106 numerical features
numeric_ftrs = colnames(train[, unlist(lapply(train, is.numeric))])
## 16 categorical features
cat_ftrs = setdiff(colnames(train), numeric_ftrs)

############ Encode categorical features #############
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}

## get the list of numeric features

# fileConn<-file("categorical_mapping.txt")
train_cat <- train[cat_ftrs]
train_cat[train_cat==''] = '<NA>'

df_encoding <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_encoding) <- c('feature', 'category', 'encoding')
for (i in 1:length(cat_ftrs)){
  # sink(file = "categorical_mapping.txt", append = T)
  # print(cat("No.", i, cat_ftrs[i]))
  encoding <- encode_ordinal(train_cat[[cat_ftrs[i]]])
  table <- table(train_cat[[cat_ftrs[i]]], encoding)
  # train[[cat_ftrs[i]]] <- encoding

  enc <- data.frame(category=rownames(table), encoding=colnames(table),
                    feature=matrix(cat_ftrs[i], length(colnames(table)), 1))
  if ('<NA>' %in% enc$category){
    enc$encoding <- enc$encoding - 1
    enc$encoding[enc$encoding == 0] <- -1
  }
  df_encoding <- rbind(df_encoding, enc)
  
  
  # map the values
  ftr = cat_ftrs[i]
  for (k in 1:nrow(enc)){
    tmp_cate = enc$category[k]
    train_cat[ftr][train_cat[ftr] == tmp_cate] = enc$encoding[k]
  }
  # print(colnames((table)), rownames(table))
  # print(cat("------------------------------------------------"))
  # sink()
}

write.csv(df_encoding, "categorical_encoding.csv")
train[cat_ftrs] = train_cat

write.csv(train, (paste(data_folder,"application_train_encoded.csv")),
          row.names = FALSE)




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
