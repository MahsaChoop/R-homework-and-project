




# Mahsa Choopannejad / student number: 98207477 / ??Final project / Data-Driven decision-making class - Spring 1401


#-------------------importing libraries---------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ISLR)
library(lubridate)
library(ggrepel)
library(tidyverse)
#-------------------Importing Data------------

h <- read.csv("HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
summary(h)
View(h)
str(h)



#------------------Data Cleaning------------------



#-------------------Visualization-----------------------

ggplot(data = h , aes(x = PerformanceRating , fill = Gender)) +
  geom_bar() +
  ggtitle ("Attrition on performance and gender") +
  labs (fill = "Gender") + 
  facet_grid(.~Attrition)


ggplot(data = h , aes(x = PerformanceRating , fill = OverTime)) +
  geom_bar() +
  ggtitle ("Attrition on performance and overtime ") +
  labs (fill = "OverTime") + 
  facet_grid(.~Attrition)


ggplot(data = h , aes(x = PerformanceRating , fill = Department)) +
  geom_bar() +
  ggtitle ("Attrition on performance and department") +
  labs (fill = "Department") + 
  facet_grid(.~Attrition)


ggplot(data = h , aes(x = PerformanceRating , fill = Attrition)) +
  geom_bar() +
  ggtitle ("Attrition on performance and department") +
  labs (fill = "Attrition") + 
  facet_grid(.~Department)

ggplot(data = h , aes(x = PerformanceRating , fill = Attrition)) +
  geom_bar() +
  ggtitle ("Attrition on performance and JobRole") +
  labs (fill = "Attrition") + 
  facet_grid(.~JobRole)


ggplot(data = h , aes(x = WorkLifeBalance , fill = OverTime)) +
  geom_bar() +
  ggtitle ("Attrition on WorkLifeBalance and overtime ") +
  labs (fill = "OverTime") + 
  facet_grid(.~Attrition)

ggplot(data = h , aes(x = EnvironmentSatisfaction , fill = OverTime)) +
  geom_bar() +
  ggtitle ("Attrition on EnvironmentSatisfaction and overtime ") +
  labs (fill = "OverTime") + 
  facet_grid(.~Attrition)

ggplot(data = h , aes(x = DistanceFromHome , fill = OverTime)) +
  geom_bar() +
  ggtitle ("Attrition on DistanceFromHome and overtime ") +
  labs (fill = "OverTime") + 
  facet_grid(.~Attrition)



#-------------------predictive analysis-----------------------
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(glmnet)
library(psych)
library(corrplot)
library(ggcorrplot)
library(PerformanceAnalytics)
library(precrec)

  #-----------------correlation analysis---------------

h_analysis <- subset(h, select=-c(EmployeeCount, StandardHours, Over18))

model.matrix(~0+., data=h_analysis) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F,
             type="lower",
             lab=TRUE,
             lab_size=2,
             #p.mat=p.mat,
             outline.color = "white",
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"))




#---------------------------------Train data set based on y= Attrition------------------------------------------------------------------------------------

#-----------------test and tran dataset---------------

h_analysis2 <- subset(h, select=-c(EmployeeCount, StandardHours, Over18,BusinessTravel,DailyRate,EmployeeNumber,HourlyRate, MonthlyRate,NumCompaniesWorked,StockOptionLevel,TrainingTimesLastYear))

set.seed(2)
split <- sample.split(h_analysis2$Attrition, SplitRatio = 0.85)
train <- subset(h_analysis2, split==T)
test <- subset(h_analysis2, split==F)


#-----------------classification on y = Attrition---------------



t_model <- rpart(Attrition ~. , data=train, method = "class", minbucket=12)
prp(t_model)

t_pred <- predict(t_model, newdata = test , type="class" )
confusionMatrix(table(test$Attrition, t_pred))


#m <- as.data.frame(t_pred)
#test$Attrition

pred_tree <- prediction(as.numeric(t_pred), test$Attrition)
perf_tree <- performance(pred_tree, "tpr", "fpr")
plot(perf_tree)
as.numeric(performance(pred_tree, "auc")@y.values)

precrec_obj <- evalmod(scores = as.numeric(t_pred), test$Attrition, mode="basic")
autoplot(precrec_obj)


#------cross validation------
library(caret)
library(e1071)

num_fold <- trainControl(method = "cv", number = 3)
cp_grid <- expand.grid(.cp=seq(0.001, 0.3, 0.0001))
train(Attrition ~. , data=train, method="rpart", trControl= num_fold, tuneGrid=cp_grid)

cv_tree <- rpart(Attrition ~., data = train, method = "class", cp= 0.0298)
prp(cv_tree)
pred_cv <- predict(cv_tree, newdata = test, type="class")
table(test$Attrition, pred_cv)

pred_cvTree <- prediction(pred_cv, test$Attrition)
perf_cvTree <- performance(pred_cvTree, "tpr", "fpr")
plot(perf_cvTree)
as.numeric(performance(pred_cvTree, "auc")@y.values)

precrec_obj <- evalmod(scores = as.numeric(pred_cv), test$Attrition, mode="basic")
autoplot(precrec_obj)


#------------logistic model------------

log_model <- glm(Attrition ~. , data=train, family = "binomial")
summary(log_model)

log_pred <- predict(log_model, newdata = test, type = "response")
table(test$Attrition, log_pred>0.5)

pred_log <- prediction(log_pred, as.numeric(test$Attrition))
perf_log <- performance(pred_log, "tpr", "fpr")
plot(perf_log)
as.numeric(performance(pred_log, "auc")@y.values)

expected_value <- ifelse(log_pred>0.5,110,0)
compare_result <- data.frame(test, c(log_pred), expected_value)


precrec_obj <- evalmod(scores = log_pred, as.numeric(test$Attrition), mode="basic")
autoplot(precrec_obj)

#-------comparison of ROC---------

plot(perf_tree, col="red")
plot(perf_log, col="blue", add=TRUE)
legend(0.01,
       0.97,
       legend=c("Performance of tree", "Performance of logistic regression"),
       col=c("red", "blue"),
       lty=1:1,
       cex=0.8)


#------random forest------


library(randomForest)

library(caret)
library(e1071)


rf_model <- randomForest(Attrition~., data=train, nodsize=5, mtry=15, ntee=50)
rf_model

# Importance plot
importance(rf_model)

# Variable importance plot
varImpPlot(rf_model)

# grid serach tuning for hyperparameters in random forest

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Attrition~., data=train , method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


library(ROCR)
pred_rf <- predict(rf_model, newdata = test, type="prob") 
head(pred_rf)
pred <- prediction(pred_rf[,2], test$Attrition)
perf_rf <- performance(pred, "tpr", "fpr")
plot(perf_rf)
as.numeric(performance(pred, "auc")@y.values)

#-------comparison of ROC---------

plot(perf_tree, col="red")
plot(perf_log, col="blue", add=TRUE)
plot(perf_rf, col="black", add=TRUE)
legend(0.01,
       0.97,
       legend=c("Performance of tree", "Performance of logistic regression", "Performance of Random forest"),
       col=c("red", "blue","black"),
       lty=1:1,
       cex=0.6)


library(randomForest)
library(mlbench)
library(caret)
library(e1071)

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

#Metric compare model is Accuracy

metric <- "Accuracy"
set.seed(123)

#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Attrition~., 
                    data=h_analysis2, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)


#mtry: Number of random variables collected at each split. In normal equal square number columns.
mtry <- sqrt(ncol(train))
#ntree: Number of trees to grow.
ntree <- 3


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')

#Random generate 15 mtry values with tuneLength = 15
set.seed(1)
rf_random <- train(Attrition~.,
                   data = h_analysis2,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random)

plot(rf_random)

#------SVM-------------------


library(e1071)

classifier = svm(formula = Attrition ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')
print(classifier)



# Predicting the Test set results
pred_svm = predict(classifier, newdata = test)

# Making the Confusion Matrix
confusionMatrix(table(test$Attrition, pred_svm))

#grid search
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(Attrition ~., data = train, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

# Predicting the Test set results
pred_svm = predict(classifier, newdata = test)

# Making the Confusion Matrix
confusionMatrix(table(test$Attrition, pred_svm))

#------------XG Boost-----------------

library(xgboost)

train_x = data.matrix(train[, -13])
train_y = train[,13]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -13])
test_y = test[, 13]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

#define final model
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 14, verbose = 0)

#use model to make predictions on test data
pred_y = predict(final, xgb_test)



importance <- xgb.importance(feature_names = colnames(xgb_train), model = model)
importance

xgb.plot.importance(importance_matrix = importance)

#prediction & confusion matrix
p = predict(final, newdata = xgb_test)
pred_y = ifelse(p > 0.5,1,0)
confusionMatrix(table(pred_y, test_y))


# plot them features! what's contributing most to our model?
xgb.plot.multi.trees(feature_names = names(test), 
                     model = final)





