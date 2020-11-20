library(tidyverse)
library(plyr)
library(caret)
library(glmnet)
library(readr)
library(MASS)
breast_cancer <- read_csv("C:/Users/lordv/Downloads/breast-cancer.data")

# Split the data into training and test set
set.seed(123)
training.samples <- breast_cancer$`no-recurrence-events` %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- breast_cancer[training.samples, ]
test.data <- breast_cancer[-training.samples, ]

# Predictor variables
Train_X <- model.matrix(`no-recurrence-events`~., train.data)[,-1]
Test_X <- model.matrix(`no-recurrence-events`~., test.data)[,-1]
# Outcome variable
Train_Y <- train.data$`no-recurrence-events`
Test_Y <- test.data$`no-recurrence-events`

#Logistic with stepwise
Train = train.data
Train$`no-recurrence-events` = as.factor(Train$`no-recurrence-events`)
model <- glm(`no-recurrence-events` ~ ., data = Train, family = binomial) %>%
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


