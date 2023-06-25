setwd("~/Desktop/DATA/R/R_Datasets")

# Question 7.3
# part a
BHousing <- read.csv("BostonHousing.csv", header = TRUE)
set.seed(1)

train.index <- sample(row.names(BHousing), 0.6*dim(BHousing)[1])
training <- BHousing[train.index, ]
valid.index <- setdiff(row.names(BHousing), train.index)
validation <- BHousing[valid.index, ]

library(caret)
train.norm <- training
valid.norm <- validation
housing.norm <- BHousing
norm.values <- preProcess(training[, 1:12], method = c("center", "scale"))
train.norm[, 1:12] <- predict(norm.values, training[, 1:12])
valid.norm[, 1:12] <- predict(norm.values, validation[, 1:12])
housing.norm[, 1:12] <- predict(norm.values, BHousing[, 1:12])

library(class)
library(ModelMetrics)
library(MLmetrics)
MeasuresOfError.df <- data.frame(k = seq(1, 5, 1), MAE = rep(0, 5), MSE = rep(0, 5), MAPE = rep(0, 5))
for (i in 1:5) {
  knn.pred <- class::knn(train.norm[, 1:12], valid.norm[, 1:12],
                  cl = train.norm[, 13], k = i)
  MeasuresOfError.df[i, 2] <- mae(as.numeric(as.character(knn.pred)), (valid.norm[, 13]))
  MeasuresOfError.df[i, 3] <- mse(as.numeric(as.character(knn.pred)), (valid.norm[, 13]))
  MeasuresOfError.df[i, 4] <- MAPE(as.numeric(as.character(knn.pred)), (valid.norm[, 13]))
}
MeasuresOfError.df
# K = 1 gives us the lowest values for MAE, MSE, and MAPE, so we will use that as our best K.  However, when we use K = 1, we should be wary of breaking our model by increasing the power of the validation/testing point.


# part b
new.housing <- data.frame(CRIM = 0.2, ZN = 0, INDUS = 7, CHAS = 0, NOX = 0.538, RM = 6, AGE = 62, DIS = 4.7, RAD = 4, TAX = 307, PTRATIO = 21, LSTAT = 10)

new.housing.norm <- predict(norm.values, new.housing)

nn.best <- class::knn(train = train.norm[, 1:12], test = new.housing.norm, cl = train.norm[, 13], k = 1)
nn.best
#We predict that MEDV will be 20.4


# part c
library(class)
library(ModelMetrics)
library(MLmetrics)
MeasuresOfError.df <- data.frame(k = seq(1, 5, 1), MAE = rep(0, 5), MSE = rep(0, 5), MAPE = rep(0, 5))
for (i in 1:5) {
  knn.pred <- class::knn(train.norm[, 1:12], train.norm[, 1:12],
                         cl = train.norm[, 13], k = i)
  MeasuresOfError.df[i, 2] <- mae(as.numeric(as.character(knn.pred)), (train.norm[, 13]))
  MeasuresOfError.df[i, 3] <- mse(as.numeric(as.character(knn.pred)), (train.norm[, 13]))
  MeasuresOfError.df[i, 4] <- MAPE(as.numeric(as.character(knn.pred)), (train.norm[, 13]))
}
MeasuresOfError.df #just look at K=1
# No error found



# part d
#The validation data error is overly optimistic compared to new data error because the validation data and training data are part of the same dataset, while the new data is not necessarily related. 


# part e 
# The disadvantage of using k-NN prediction is the amount of time it would take to run on larger datasets.

# To find k-NN:
# Find the distance between the new data point and all the existing points from our training data
# Using those distances, sort them in increasing order
# Select the K number of point(s) that are closest
# Make a prediction for the new point using the output (if K=1) or average of outputs (if K>1) of the points selected in the previous step