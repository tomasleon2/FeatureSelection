library(tidyverse)
library(plyr)
library(caret)
library(glmnet)
library(readr)
library(MASS)
breast_cancer <- read.csv("C:/Users/lordv/Downloads/breast-cancer.data", header=FALSE)
colnames(breast_cancer) <- c("target","V1","v2","v3","v4","v5","v6","v7","v8","v9")

# Split the data into training and test set
set.seed(123)
training.samples <- breast_cancer$target %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- breast_cancer[training.samples, ]
test.data <- breast_cancer[-training.samples, ]

# Predictor variables
Train_X <- model.matrix(target~., train.data)[,-1]
Test_X <- model.matrix(target~., test.data)[,-1]
# Outcome variable
Train_Y <- train.data$target
Test_Y <- test.data$target

#Logistic with stepwise
Train = train.data
Train$target = as.factor(Train$target)
model <- glm(target ~ ., data = Train, family = binomial) %>%
  stepAIC(trace = FALSE)
coef(model)

#Lasso
cv1 <- cv.glmnet(Train_X, Train_Y, family = "binomial", nfold = 10, type.measure = "deviance", alpha = 1)
md1 <- glmnet(Train_X, Train_Y, family = "binomial", lambda = cv1$lambda.1se, alpha = 1)
coef(md1)

#Ridge
cv2 <- cv.glmnet(Train_X, Train_Y, family = "binomial", nfold = 10, type.measure = "deviance", alpha = 0)
md2 <- glmnet(Train_X, Train_Y, family = "binomial", lambda = cv2$lambda.1se, alpha = 0)
coef(md2)

#ElasticNet
a <- as.list(seq(0.1, 0.9, 0.2))
EN <- function(i){
  cv <- cv.glmnet(Train_X, Train_Y, family = "binomial", nfold = 10, type.measure = "deviance", alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
search <- as.data.frame(t(sapply(a, EN)))

cv3 <- search[unlist(search$cvm) == min(unlist(search$cvm)), ]
md3 <- glmnet(Train_X, Train_Y, family = "binomial", lambda = cv3$lambda.1se[[1]], alpha = cv3$alpha[[1]])
coef(md3)


