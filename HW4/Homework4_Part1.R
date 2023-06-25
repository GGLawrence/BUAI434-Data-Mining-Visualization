#####################################################
## Chapter 9 - Classification and Regression Trees ##
#####################################################

#NOTE: Prepared with R version 3.6.0

#set the working directory to appropriate folder on your machine, so as to
#access the data files.

#load the required libraries/packages for this chapter
#Install the package(s) below once on your machine. To do so, uncomment the
#install.packages line(s) below.

#instal.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("xgboost")
#install.packages("Matrix")
#install.packages("caret")
#install.packages("gains")
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)
library(caret)
library(gains)

## Problem 1 Competitive Auctions on eBay.com.
##The file eBayAuctions.csv contains information on 1972 auctions that
##transacted on eBay.com during May-June 2004. The goal is to use these data to
##build a model that will classify auctions as competitive or noncompetitive.
##A competitive auction is defined as an auction with at least two bids placed
##on the item auctioned. The data include variables that describe the item
##(auction category), the seller (his/her eBay rating), and the auction terms
##that the seller selected (auction duration, opening price, currency,
##day-of-week of auction close). In addition, we have the price at which the
##auction closed. The task is to predict whether or not the auction will be
##competitive.
##Data Preprocessing. Convert variable Duration into a categorical variable.
##Split the data into training (60%) and validation (40%) datasets.

#load the data
ebay.df <- read.csv("/Users/zeyaoyang/Downloads/Datasets/eBayAuctions.csv")
lapply(ebay.df, class)
#convet variable "Duration" to categorical type
ebay.df$Duration <- as.factor(ebay.df$Duration)
ebay.df$Competitive. <- as.factor(ebay.df$Competitive.)

#partition the data into train (60%) and validation (40%) sets
#set the seed for the random number generator for reproducing the partition.

set.seed(1)
sub <- sample(1:nrow(ebay.df), round(nrow(ebay.df) * 0.6))
ebay_train <- ebay.df[sub,]
ebay_val <- ebay.df[-sub,]





##1.a Fit a classification tree using all predictors, using the best-pruned
##tree. To avoid overfitting, set the minimum number of records in a terminal
##node to 50 (in R: minbucket = 50). Also, set the maximum number of levels to
##be displayed at seven (in R: maxdepth = 7). Write down the results in terms
##of rules. (Note: If you had to slightly reduce the number of predictors due
##to software limitations, or for clarity of presentation, which would be a
##good variable to choose?)

#fit a classification tree model

model = rpart(
  Competitive. ~ .,
  data = ebay_train,
  method = 'class',
  minbucket = 50,
  maxdepth = 7
)



#which are the most important variables?

m <- summary(model)
m$variable.importance
# The most important variables are ClosePrice, OpenPrice, Category.

#set of rules:

rpart.rules(model)


# Competitive.
# 0.18 when OpenPrice <  3.6         & ClosePrice <   3.6 & Category is Antique/Art/Craft or Automotive or Books or Clothing/Accessories or Coins/Stamps or Collectibles or Computer or EverythingElse or Health/Beauty or Pottery/Glass or SportingGoods or Toys/Hobbies
# 0.20 when OpenPrice >=         3.6 & ClosePrice <  12.3
# 0.27 when OpenPrice >=        12.1 & ClosePrice >= 12.3 & sellerRating >= 553
# 0.53 when OpenPrice <  3.6         & ClosePrice <   3.6 & Category is Business/Industrial or Electronics or Home/Garden or Jewelry or Music/Movie/Game
# 0.66 when OpenPrice >=        12.1 & ClosePrice >= 12.3 & sellerRating <  553
# 0.83 when OpenPrice is 3.6 to 12.1 & ClosePrice >= 12.3
# 0.99 when OpenPrice <  3.6         & ClosePrice >=  3.6



##1.b Describe the interesting and uninteresting information that these rules
##provide.

# When the openprice is less than 3.6 and the closeprice is greater than 3.6 it is almost certain to be competitive.
# When the seller rating is low, the auction is more likely to be non-competitive.
# If the category does not belong to  Business/Industrial or Electronics or Home/Garden or Jewelry or Music/Movie/Game,
#the auction is likely to be non-competitive.



## 1.c Fit another classification tree (using the best-pruned tree, with a
##minimum number of records per terminal node = 50 and maximum allowed number
##of displayed levels = 7), this time only with predictors that can be used for
##predicting the outcome of a new auction. Describe the resulting tree in terms
##of rules. Make sure to report the smallest set of rules required for
##classification.


new_predictors <- setdiff(colnames(ebay_train), 'ClosePrice')
model2 = rpart(
  Competitive. ~ .,
  data = ebay_train[new_predictors],
  method = 'class',
  minbucket = 50,
  maxdepth = 7
)




#set of rules

rpart.rules(model2)
# Competitive.
# 0.21 when OpenPrice >= 3.6 & sellerRating >=  557 & Category is Antique/Art/Craft or Automotive or Clothing/Accessories or Coins/Stamps or Computer or EverythingElse or Health/Beauty or Home/Garden or Jewelry or Music/Movie/Game or Photography or Pottery/Glass
# 0.34 when OpenPrice >= 3.6 & sellerRating >=  557 & Category is Books or Business/Industrial or Collectibles or Electronics or SportingGoods or Toys/Hobbies & endDay is Fri or Sat or Thu
# 0.39 when OpenPrice <  3.6 & sellerRating >= 2428 & Duration is 7 or 10
# 0.61 when OpenPrice >= 3.6 & sellerRating >=  557 & Category is Books or Business/Industrial or Collectibles or Electronics or SportingGoods or Toys/Hobbies & endDay is Mon or Sun or Tue or Wed
# 0.62 when OpenPrice >= 3.6 & sellerRating <   557




##1.d Examine the confusion matrix for the tree. What can
##you say about the predictive performance of this model?


predict_test = predict(model2, ebay_val, type = "class")
confusion_matrix = confusionMatrix(data = predict_test, reference = ebay_val$Competitive.)

#             Reference
# Prediction   0   1
#     0       234 103
#     1       126 326

# The accuracy of the model is 70.98%.
# The model is relatively effective in predicting whether the new auction is a competitive.







## 1.e Based on this last tree, what can you conclude from these data about
##the chances of an auction obtaining at least two bids and its relationship to
##the auction settings set by the seller (duration, opening price, ending day,
##currency)? What would you recommend for a seller as the strategy that will
##most likely lead to a competitive auction?

# When duration is 7 or 10, the auction tend to be non-competitive.
# When the end day is on Thursday to Saturday, the chances of an auction obtaining only one bid is higher.
# I would recommend for a seller that setting the auction duration less than a week and the end day on Sunday to Wednesday.





#############################


#############################

## Problem 2 Predicting Prices of Used Cars (Regression Trees). The file
##ToyotaCorolla.csv contains the data on used cars (Toyota Corolla) on sale
##during late summer of 2004 in the Netherlands. It has 1436 records containing
##details on 38 attributes, including Price, Age, Kilometers, HP, and other
##specifications. The goal is to predict the price of a used Toyota Corolla
##based on its specifications.
##Data Preprocessing. Split the data into training (60%), and validation (40%)
##datasets.

car.df <- read.csv("/Users/zeyaoyang/Downloads/Datasets/ToyotaCorolla.csv")

# preprocess

sub2 <- sample(1:nrow(car.df), round(nrow(car.df) * 0.6))
car_train <- car.df[sub2,]
car_val <- car.df[-sub2,]




##2.a Run a regression tree (RT) with outcome variable Price and predictors
##Age_08_04, KM, Fuel_Type, HP, Automatic, Doors, Quarterly_Tax, Mfr_Guarantee,
##Guarantee_Period, Airco, Automatic_Airco, CD_Player, Powered_Windows,
##Sport_Model, and Tow_Bar. Keep the minimum number of records in a terminal
##node to 1, maximum number of tree levels to 30, and cp = 0:001, to make the
##run least restrictive.

####For regression tree, you need to set: method = "anova" #####

# regression tree:

predictors_car <-
  c(
    'Price',
    'Age_08_04',
    'KM',
    'Fuel_Type',
    'HP',
    'Automatic',
    'Doors',
    'Quarterly_Tax',
    'Mfr_Guarantee',
    'Guarantee_Period',
    'Airco',
    'Automatic_airco',
    'CD_Player',
    'Powered_Windows',
    'Sport_Model',
    'Tow_Bar'
  )
model_car = rpart(
  Price ~ .,
  data = car_train[predictors_car],
  method = 'anova',
  minbucket = 1,
  maxdepth = 30,
  cp = 0.001
)






## 2.b Which appear to be the three or four most important car specifications
##for predicting the car's price?

#variable importance


m_car <- summary(model_car)
m_car$variable.importance

# The three most important car specifications for predicting the car's price are Age_08_04, KM, Automatic_airco.









## 2.c Compare the prediction errors of the training and validation sets by
##examining their RMS error. How does the predictive performance of the validation set compare to the training set?
##Why does this occur?


#install.packages("forecast")

library(forecast)

accuracy(predict(model_car, car_train), car_train$Price)
#                 ME     RMSE      MAE        MPE     MAPE
# Test set 1.262416e-13 938.9401 734.7888 -0.8994041 7.416472

accuracy(predict(model_car, car_val), car_val$Price)
#                ME     RMSE      MAE       MPE     MAPE
# Test set -105.322 1282.971 944.2944 -2.144833 9.227761

# RMS error of the validation sets is greater than the training sets.

# This model performs better on the training data than on the unknown validation data since a bit of overfitting occured.








## 2.d How might we achieve better validation predictive performance at
##the expense of training performance?

# We can reduce the proportion of samples used as training set







## 2.e Create a less deep tree by leaving the arguments cp, minbucket, and
##maxdepth at their defaults. Compared to the deeper tree, what is the
##predictive performance on the validation set?

less_deep_tree <- rpart(Price ~ ., data = car_train[predictors_car], method = 'anova', )
accuracy(predict(less_deep_tree, car_train), car_train$Price)
#                    ME     RMSE      MAE       MPE     MAPE
#Test set -4.623794e-13 1359.948 1026.068 -1.704403 10.16043
accuracy(predict(less_deep_tree, car_val), car_val$Price)
#                ME     RMSE      MAE       MPE    MAPE
#Test set - 98.62657 1469.897 1073.785 - 2.624632 10.4939

# Although the difference in RMS between train and validation sets is smaller, the errors have all increased.

