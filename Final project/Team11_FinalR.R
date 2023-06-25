dir()
getwd()
GC.df <- read.csv( "GermanCredit.csv", header = TRUE)
GC.df
#refining GC.df

#remove unwanted GC.df
GC.df <- GC.df[, -c(1,6,8,10,20,26,29,30,31)]
GC.df
#GC.df[, -c(1,15,16,17,22,25)]
#convert some fields to category fields as per description 

GC.df$CHK_ACCT = factor(GC.df$CHK_ACCT, levels = c(0,1,2,3))
GC.df$CHK_ACCT
GC.df$HISTORY = factor(GC.df$HISTORY, levels = c(0,1,2,3,4))
GC.df$NEW_CAR = factor(GC.df$NEW_CAR, levels = c(0,1))
GC.df$NEW_CAR

GC.df$USED_CAR = factor(GC.df$USED_CAR, levels = c(0,1))
 
GC.df$FURNITURE = factor(GC.df$FURNITURE, levels = c(0,1))
GC.df$RADIO.TV = factor(GC.df$RADIO.TV, levels = c(0,1))
GC.df$EDUCATION = factor(GC.df$EDUCATION)
GC.df$RETRAINING = factor(GC.df$RETRAINING, levels = c(0,1))
GC.df$SAV_ACCT = factor(GC.df$SAV_ACCT, levels = c(0,1,2,3,4))
GC.df$EMPLOYMENT = factor(GC.df$EMPLOYMENT, levels = c(0,1,2,3,4))
GC.df$MALE_DIV = factor(GC.df$MALE_DIV, levels = c(0,1))
GC.df$MALE_SINGLE = factor(GC.df$MALE_SINGLE, levels = c(0,1))
GC.df$MALE_MAR_or_WID = factor(GC.df$MALE_MAR_or_WID, levels = c(0,1))
GC.df$CO.APPLICANT = factor(GC.df$CO.APPLICANT, levels = c(0,1))
GC.df$GUARANTOR = factor(GC.df$GUARANTOR, levels = c(0,1))
GC.df$PRESENT_RESIDENT = factor(GC.df$PRESENT_RESIDENT)
GC.df$REAL_ESTATE = factor(GC.df$REAL_ESTATE, levels = c(0,1))
GC.df$PROP_UNKN_NONE = factor(GC.df$PROP_UNKN_NONE, levels = c(0,1))
GC.df$OTHER_INSTALL = factor(GC.df$OTHER_INSTALL, levels = c(0,1))
GC.df$RENT= factor(GC.df$RENT, levels = c(0,1))
GC.df$OWN_RES = factor(GC.df$OWN_RES, levels = c(0,1))
GC.df$JOB = factor(GC.df$JOB, levels = c(0,1,2,3))
GC.df$TELEPHONE = factor(GC.df$TELEPHONE, levels = c(0,1))
GC.df$FOREIGN = factor(GC.df$FOREIGN, levels = c(0,1))
GC.df$RESPONSE = factor(GC.df$RESPONSE, levels = c(0,1))

#display summary
summary(GC.df)
library(survival)
library(Hmisc)
describe(GC.df)
library(caret)

#splitting the data into 60% training and 40% validation data
set.seed(12345) #in order to randomize
index <- createDataPartition(GC.df$RESPONSE, p=0.60, list = FALSE)
training <- GC.df[index,]
validation <- GC.df[-index ,]

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
GC.norm.df <- GC.df
# use preProcess() from the caret package to normalize 
norm.values <- preProcess(train.df[, c(2,7,10,18)], method=c("center", "scale"))
train.norm.df[, c(2,7,10,18)] <- predict(norm.values, train.df[, c(2,7,10,18)])
valid.norm.df[, c(2,7,10,18)] <- predict(norm.values, valid.df[, c(2,7,10,18)])
GC.norm.df[, c(2,7,10,18)] <- predict(norm.values, GC.df[, c(2,7,10,18)])

#model with every factors
GC.whole.df <- read.csv("/Users/liuruiqi/Desktop/case western/data visualization/final project/GermanCredit.csv")

GC.whole.df = subset(GC.whole.df, select = -c(1) )

#partioning the data
set.seed(12345)
train.index <- sample(row.names(GC.whole.df), 0.6*dim(GC.whole.df)[1])  
valid.index <- setdiff(row.names(GC.whole.df), train.index)  
train.whole.df <- GC.whole.df[train.index, ]
valid.whole.df <- GC.whole.df[valid.index, ]

train.whole.norm.df <- train.whole.df
valid.whole.norm.df <- valid.whole.df
GC.whole.norm.df <- GC.whole.df

#normalize the numerical data
norm.whole.values <- preProcess(train.whole.df[, c(2,10,13,22,26,28)], method=c("center", "scale"))
train.whole.norm.df[, c(2,10,13,22,26,28)] <- predict(norm.whole.values, train.whole.df[, c(2,10,13,22,26,28)])
valid.whole.norm.df[, c(2,10,13,22,26,28)] <- predict(norm.whole.values, valid.whole.df[, c(2,10,13,22,26,28)])
GC.whole.norm.df[, c(2,10,13,22,26,28)] <- predict(norm.whole.values, GC.whole.df[, c(2,10,13,22,26,28)])


#####Classification Tree#####
library(rpart)
library(rpart.plot)

class.tree <- rpart(RESPONSE ~ ., data = training, control = rpart.control(maxdepth = 30), method = "class")
class.tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

#predicting data in validation data
class.tree.predicted.data <- predict(class.tree, validation, type = "class")
table(predicted = class.tree.predicted.data, actual = validation$RESPONSE)


#####Logistic Regression#####
GC.df.Logistic <- read.csv("GermanCredit.csv")
GC.df.Logistic <- GC.df.Logistic[, -c(1,15,16,17,22,25)]
#View(GC.df.Logistic)
#####Training and validation
set.seed(1234)
train.index <- sample(c(1:dim(GC.df.Logistic)[1]), dim(GC.df.Logistic)[1]*0.6)
train.df <- GC.df.Logistic[train.index,]
valid.df <- GC.df.Logistic[-train.index,]
logistic.GC <- glm(RESPONSE ~ ., data = train.df, family = "binomial")
logistic.GC
options(scipen = 999)
summary(logistic.GC)
#predicting data in validation data
default.ct <- rpart(RESPONSE ~., data = train.df, method = "class")
prp(default.ct, type = 1, under = TRUE, split.font = 1, varlen = -10)
predicted.data <- predict(default.ct, valid.df, type = "class")
table(predicted = predicted.data, actual = valid.df$RESPONSE)
plot(logistic.GC)

#plot(logistic.GC)
#Hit <Return> to see next plot: default.ct <- rpart(RESPONSE ~., data = train.df, method = "class")
#Hit <Return> to see next plot: prp(default.ct, type = 1, under = TRUE, split.font = 1, varlen = -10)
##


#########   KNN with 1st datasets

GC.df <- read.csv("GermanCredit.csv")
#6.delete usedcar (new car)
#8 delete radio/tv (furniture)
#10.delete retraining (education)
#20present resident;low correlation to responsive variable
#26.own-res(real-estate)
#29.people for whom liable to provide maintenance; low correlation
#30.telephone;low corrlation
#31.foreign worker ; low observation

#model with selected variables
GC.df = subset(GC.df, select = -c(1,6,8,10,20,26,29,30,31) )

#seperate the data

set.seed(12345)
train.index <- sample(row.names(GC.df), 0.6*dim(GC.df)[1])  
valid.index <- setdiff(row.names(GC.df), train.index)  
train.df <- GC.df[train.index, ]
valid.df <- GC.df[valid.index, ]



# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
GC.norm.df <- GC.df
# use preProcess() from the caret package to normalize 
norm.values <- preProcess(train.df[, c(2,7,10,18)], method=c("center", "scale"))
train.norm.df[, c(2,7,10,18)] <- predict(norm.values, train.df[, c(2,7,10,18)])
valid.norm.df[, c(2,7,10,18)] <- predict(norm.values, valid.df[, c(2,7,10,18)])
GC.norm.df[, c(2,7,10,18)] <- predict(norm.values, GC.df[, c(2,7,10,18)])



library(caret)

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 44, 1), accuracy = rep(0, 44))

# compute knn for different k on validation.
for(i in 1:44) {
  
  knn.pred <- knn(train.norm.df[,1:22], valid.norm.df[,1:22], 
                  cl = train.norm.df[,23], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(valid.norm.df[, 23]))$overall[1] 
}
accuracy.df
plot(accuracy.df)

#k=24,28,29

knn.pred1 <- knn(train.norm.df[,1:22], valid.norm.df[,1:22], cl=train.norm.df[,23], k=24)
t1 <- confusionMatrix(knn.pred1, as.factor(valid.norm.df[, 23]))$overall[1]

table(predicted=knn.pred1, actual=valid.norm.df$RESPONSE)
table(actual=valid.norm.df$RESPONSE,predicted=knn.pred1)

#model with every factors
GC.whole.df <- read.csv("GermanCredit.csv")

GC.whole.df = subset(GC.whole.df, select = -c(1) )

#partioning the data
set.seed(12345)
train.index <- sample(row.names(GC.whole.df), 0.6*dim(GC.whole.df)[1])  
valid.index <- setdiff(row.names(GC.whole.df), train.index)  
train.whole.df <- GC.whole.df[train.index, ]
valid.whole.df <- GC.whole.df[valid.index, ]

train.whole.norm.df <- train.whole.df
valid.whole.norm.df <- valid.whole.df
GC.whole.norm.df <- GC.whole.df

#normalize the numerical data
norm.whole.values <- preProcess(train.whole.df[, c(2,10,13,22,26,28)], method=c("center", "scale"))
train.whole.norm.df[, c(2,10,13,22,26,28)] <- predict(norm.whole.values, train.whole.df[, c(2,10,13,22,26,28)])
valid.whole.norm.df[, c(2,10,13,22,26,28)] <- predict(norm.whole.values, valid.whole.df[, c(2,10,13,22,26,28)])
GC.whole.norm.df[, c(2,10,13,22,26,28)] <- predict(norm.whole.values, GC.whole.df[, c(2,10,13,22,26,28)])



library(caret)

# initialize a data frame with 44 columns: k, and accuracy.
accuracy.whole.df <- data.frame(k = seq(1, 44, 1), accuracy = rep(0, 44))

# compute knn for different k on validation.
for(i in 1:44) {
  
  knn.whole.pred <- knn(train.whole.norm.df[,1:30], valid.whole.norm.df[,1:30], 
                        cl = train.whole.norm.df[,31], k = i)
  accuracy.whole.df[i, 2] <- confusionMatrix(knn.whole.pred, as.factor(valid.whole.norm.df[, 31]))$overall[1] 
}
accuracy.whole.df
plot(accuracy.whole.df)

#k=21,23,26

knn.pred.whole <- knn(train.whole.norm.df[,1:30], valid.whole.norm.df[,1:30], cl=train.whole.norm.df[,31], k=21)
t2 <- confusionMatrix(knn.pred.whole, as.factor(valid.whole.norm.df[, 31]))$overall[1]

table(predicted=knn.pred.whole, actual=valid.whole.norm.df$RESPONSE)

table(actual=valid.whole.norm.df$RESPONSE,predicted=knn.pred.whole )

#better accuracy for using every factors

