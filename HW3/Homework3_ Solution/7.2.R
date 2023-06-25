setwd("~/Desktop/DATA/R/R_Datasets")

#Question 7.2
#part a
bank.df <- read.csv("UniversalBank.csv", header = TRUE)
bank.df <- subset(bank.df, select = -c(ID, ZIP.Code))
bank.df <- as.data.frame(bank.df)

bank.df$Edu1 <- ifelse(bank.df$Education == 1,1,0)
bank.df$Edu2 <- ifelse(bank.df$Education == 2,1,0)
bank.df$Edu3 <- ifelse(bank.df$Education == 3,1,0)
bank.df <- subset(bank.df, select = -c(Education))

customer <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Edu1 = 0, Edu2 = 1, Edu3 = 0, Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1)

library(FNN)
set.seed(1)
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1])
training <- bank.df[train.index, ]
valid.index <- setdiff(row.names(bank.df), train.index)
validation <- bank.df[valid.index, ]
train.norm <- training
valid.norm <- validation
bank.norm <- bank.df

library(caret)
norm.values <- preProcess(training[, c(1:6, 8:14)], method = c("center", "scale"))
train.norm[, c(1:6, 8:14)] <- predict(norm.values, training[, c(1:6, 8:14)])
valid.norm[, c(1:6, 8:14)] <- predict(norm.values, validation[, c(1:6, 8:14)])
bank.norm[, c(1:6, 8:14)] <- predict(norm.values, bank.df[, c(1:6, 8:14)])
customer.norm <- predict(norm.values, customer)

library(FNN)
nn <- FNN::knn(train = train.norm[, c(1:6, 8:14)], test = customer.norm, cl = train.norm[, 7], k = 1)
training[row.names(training)[attr(nn, "nn.index")], 7]

# 1 -> Does take a personal loan


##part b
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

library(FNN)
library(class)

for(i in 1:14) {
  knn.pred <- FNN::knn(train.norm[, c(1:6, 8:14)], valid.norm[, c(1:6, 8:14)],
                  cl = train.norm[, 7], k = i)
  accuracy.df[i, 2] <- caret::confusionMatrix(factor(knn.pred), factor(valid.norm[, 7]))$overall[1]
}
accuracy.df

#K=3 with 96.4% accuracy has the best balance between overfitting and ignoring the prediction information


##part c
best.k.pred <- knn(train.norm[, c(1:6, 8:14)], valid.norm[, c(1:6, 8:14)],
                     cl = train.norm[, 7], k = 3)
caret::confusionMatrix(factor(best.k.pred), factor(valid.norm[, 7]))


##part d
library(FNN)
best <- FNN::knn(train = train.norm[, c(1:6, 8:14)], test = customer.norm, cl = train.norm[, 7], k = 3)
training[row.names(training)[attr(best, "nn.index")], 7]
#Propensity of .33 -> Would not accept the loan

##part e
library(FNN)
set.seed(1)

retrain.index <- sample(row.names(bank.df), 0.5*dim(bank.df)[1])
retraining <- bank.df[retrain.index, ]
revalid.index <- sample(setdiff(row.names(bank.df), retrain.index), 0.3*dim(bank.df)[1])
revalidation <- bank.df[revalid.index, ]
retest.index <- setdiff(row.names(bank.df), union(retrain.index, revalid.index))
retest <- bank.df[retest.index, ]

library(caret)
retrain.norm <- retraining
revalid.norm <- revalidation
retest.norm <- retest
rebank.norm <- bank.df
renorm.values <- preProcess(retraining[, c(1:6, 8:14)], method = c("center", "scale"))
retrain.norm[, c(1:6, 8:14)] <- predict(renorm.values, retraining[, c(1:6, 8:14)])
revalid.norm[, c(1:6, 8:14)] <- predict(renorm.values, revalidation[, c(1:6, 8:14)])
retest.norm[, c(1:6, 8:14)] <- predict(renorm.values, retest[, c(1:6, 8:14)])
rebank.norm[, c(1:6, 8:14)] <- predict(renorm.values, bank.df[, c(1:6, 8:14)])

nn_retrain <- knn(train = retrain.norm[, c(1:6, 8:14)], test = retrain.norm[, c(1:6, 8:14)], cl = retrain.norm[, 7], k = 3)
caret::confusionMatrix(factor(nn_retrain), factor(retrain.norm[, 7]))
nn_revalid <- knn(train = retrain.norm[, c(1:6, 8:14)], test = revalid.norm[, c(1:6, 8:14)], cl = retrain.norm[, 7], k = 3)
caret::confusionMatrix(factor(nn_revalid), factor(revalid.norm[, 7]))
nn_retest <- knn(train = retrain.norm[, c(1:6, 8:14)], test = retest.norm[, c(1:6, 8:14)], cl = retrain.norm[, 7], k = 3)
caret::confusionMatrix(factor(nn_retest), factor(retest.norm[, 7]))

#Our accuracy for the training is slightly better than our validation, which is slightly better than our test set.  The model is likely to have a higher training accuracy because the model is built on the training data; however, our minimal dropoff between training and validation/testing means that overfitting is not a concern.  It is possible that the slight difference in the accuracy of the validation and the testing datasets is due to the nature of the sample and/or the smaller sample size.