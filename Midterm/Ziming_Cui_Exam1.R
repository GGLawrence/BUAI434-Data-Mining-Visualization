test <- read.csv("Airfares.csv", header = TRUE)  # load data
mean <- mean(test$FARE)
median <- median(test$FARE)
standard_dev <- sd(test$FARE)
test1 <- data.frame(mean,median,standard_dev)

set.seed(1)
train.rows <- sample(rownames(test), dim(test)[1]*0.7)
train.data <- test[train.rows, ]
valid.rows <- setdiff(rownames(test), train.rows) 
valid.data <- test[valid.rows, ]

reg <- lm(FARE ~ DISTANCE, data = test, subset = train.rows)
tr.res <- data.frame(train.data$FARE, reg$fitted.values, reg$residuals)
head(tr.res)

pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$FARE, pred, residuals = 
                       valid.data$FARE - pred)
head(vl.res)
MAPE <- mean(abs((vl.res$residuals)/vl.res$valid.data.FARE))*100

library(forecast)
accuracy(pred, valid.data$FARE)