library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)

heart <- read.csv("Heart.csv", stringsAsFactors = T)
str(heart)
heart <- na.omit(heart)
heart <- heart[,2:15]

set.seed(2)
split <- sample.split(heart$AHD, SplitRatio = 0.8)
train <- subset(heart, split==T)
test <- subset(heart, split==F)

t_model <- rpart(AHD ~. , data=train, method = "class", minbucket=2)
prp(t_model)

t_pred <- predict(t_model, newdata = test , type="class" )
table(test$AHD, t_pred)

t_pred_train <- predict(t_model, type = "class" )
table(train$AHD, t_pred_train)

log_model <- glm(AHD ~. , data=train, family = "binomial")
summary(log_model)

log_pred <- predict(log_model, newdata = test, type = "response")
table(test$AHD, log_pred>0.5)

library(ROCR)
pred_tree <- prediction(t_pred[,2], test$AHD)
perf_tree <- performance(pred_tree, "tpr", "fpr")
plot(perf_tree)
as.numeric(performance(pred_tree, "auc")@y.values)


pred_log <- prediction(log_pred, test$AHD)
perf_log <- performance(pred_log, "tpr", "fpr")
plot(perf_log)
as.numeric(performance(pred_log, "auc")@y.values)

plot(perf_tree, col="red")
plot(perf_log, col="blue", add=TRUE)



split <- sample.split(train$AHD, SplitRatio = 0.5)
train1 <- subset(train, split==T)
train2 <- subset(train, split==F)


t_model <- rpart(AHD ~. , data=train2, method = "class", minbucket=2)
prp(t_model)

library(randomForest)
rf_model <- randomForest(AHD~., data=train, nodsize=5, mtry=4, ntee=500)
rf_model

library(ROCR)
pred_rf <- predict(rf_model, newdata = test, type="prob") 
head(pred_rf)
pred <- prediction(pred_rf[,2], test$AHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

library(caret)
library(e1071)

num_fold <- trainControl(method = "cv", number = 3)
cp_grid <- expand.grid(.cp=seq(0.001, 0.3, 0.0001))
train(AHD ~. , data=train, method="rpart", trControl= num_fold, tuneGrid=cp_grid)
cv_tree <- rpart(AHD ~., data = train, method = "class", cp= )
prp(cv_tree)
pred_cv <- predict(cv_tree, newdata = test, type="class")
table(test$AHD, pred_cv)
