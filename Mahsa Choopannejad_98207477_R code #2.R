

# Mahsa Choopannejad / student number: 98207477 / Exercise #2 / Data-Driven decision-making class - Spring 1401


#-------------------importing libraries---------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ISLR)
library(lubridate)
library(ggrepel)
library(tidyverse)
#-------------------Importing Data------------

answers <- read.csv("AdviseInvestData.csv" ,stringsAsFactors = T)
summary(answers)
View(answers)
str(answers)

answers$ANSWERED <- as.factor(answers$ANSWERED)
answers$FEMALE <- as.factor(answers$FEMALE)
answers$JOB <- as.factor(answers$JOB)
answers$RENT <- as.factor(answers$RENT)
answers$OWN_RES <- as.factor(answers$OWN_RES)
answers$NEW_CAR <- as.factor(answers$NEW_CAR)
answers$MOBILE <- as.factor(answers$MOBILE)
answers$PRODUCT <- as.factor(answers$PRODUCT)
answers$NUM_DEPENDENTS <- as.factor(answers$NUM_DEPENDENTS)
answers$CHK_ACCT <- as.factor(answers$CHK_ACCT)
answers$SAV_ACCT <- as.factor(answers$SAV_ACCT)
answers$NUM_ACCTS <- as.factor(answers$NUM_ACCTS)

summary(answers)
View(answers)
str(answers)
#------------------Data Cleaning------------------

answers <- na.omit(answers)
invalid_age <- subset(answers, answers$AGE<0)
invalid_age

#-------------------Visualization-----------------------

hist(answers$AGE)

ggplot(data = answers, aes( x =factor(1), fill = FEMALE))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("customers based on gender")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel2")+
  guides(fill = guide_legend(title = "Is female?")) +
  theme_minimal()

ggplot(data = answers , aes(x = ANSWERED , fill = FEMALE)) +
  geom_bar() +
  ggtitle ("answers in each Gender") 

ggplot(data = answers , aes(x = ANSWERED , fill = MOBILE)) +
  geom_bar() +
  ggtitle ("answers with provided mobile number or not") 

ggplot(data = answers , aes(x = ANSWERED , fill = JOB)) +
  geom_bar() +
  ggtitle ("answers with nature of job") +
  scale_fill_brewer(palette = "Pastel2")

hist(answers$INCOME)
ggplot(data = answers , aes(x = INCOME , fill = ANSWERED, color = ANSWERED)) +
  geom_histogram(alpha=0.8) +
  ggtitle ("answers with income histogram")

ggplot(data = answers , aes(x = PRODUCT , fill = ANSWERED)) +
  geom_bar() +
  ggtitle ("answers with product type") +
  scale_fill_brewer(palette = "Pastel2")

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

answers_excludeProduct <- subset(answers, select=-c(OBS.,PRODUCT))
Products_excludeAnswer <- subset(answers, select=-c(OBS.,ANSWERED))

      #-----------------correlation analysis---------------

model.matrix(~0+., data=answers_excludeProduct) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F,
             type="lower",
             lab=TRUE,
             lab_size=2,
             #p.mat=p.mat,
             outline.color = "white",
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"))

#chart.Correlation(answers_excludeProduct, histogram=TRUE, pch=19)

#answers_excludeProduct.cor = cor(answers_excludeProduct, method = c("spearman"))
#corrplot(answers_excludeProduct.cor)
#cor_matrix_answers_excludeProduct <- cor(answers_excludeProduct[,2])
#heatmap(cor_matrix_answers_excludeProduct)



#---------------------------------Train data set based on y= answered------------------------------------------------------------------------------------

      #-----------------test and tran dataset---------------
set.seed(2)
split <- sample.split(answers_excludeProduct$ANSWERED, SplitRatio = 0.9)
train <- subset(answers_excludeProduct, split==T)
test <- subset(answers_excludeProduct, split==F)


      #-----------------classification on y = answered---------------
t_model <- rpart(ANSWERED ~. , data=train, method = "class", minbucket=5)
prp(t_model)

t_pred <- predict(t_model, newdata = test , type="class" )
table(test$ANSWERED, t_pred)


#m <- as.data.frame(t_pred)
#test$ANSWERED

pred_tree <- prediction(as.numeric(t_pred), test$ANSWERED)
perf_tree <- performance(pred_tree, "tpr", "fpr")
plot(perf_tree)
as.numeric(performance(pred_tree, "auc")@y.values)

precrec_obj <- evalmod(scores = as.numeric(t_pred), test$ANSWERED, mode="basic")
autoplot(precrec_obj)

  #------------logistic model------------

log_model <- glm(ANSWERED ~. , data=train, family = "binomial")
summary(log_model)

log_pred <- predict(log_model, newdata = test, type = "response")
table(test$ANSWERED, log_pred>0.5)

pred_log <- prediction(log_pred, as.numeric(test$ANSWERED))
perf_log <- performance(pred_log, "tpr", "fpr")
plot(perf_log)
as.numeric(performance(pred_log, "auc")@y.values)

expected_value <- ifelse(log_pred>0.5,110,0)
compare_result <- data.frame(test, c(log_pred), expected_value)


precrec_obj <- evalmod(scores = log_pred, as.numeric(test$ANSWERED), mode="basic")
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
rf_model <- randomForest(ANSWERED~., data=train, nodsize=5, mtry=11, ntee=50)
rf_model

      # Importance plot
importance(rf_model)

      # Variable importance plot
varImpPlot(rf_model)

      # grid serach tuning for hyperparameters in random forest
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:5))
rf_gridsearch <- train(ANSWERED~., data=train , method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


library(ROCR)
pred_rf <- predict(rf_model, newdata = test, type="prob") 
head(pred_rf)
pred <- prediction(pred_rf[,2], test$ANSWERED)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

  #------cross validation------
library(caret)
library(e1071)

num_fold <- trainControl(method = "cv", number = 3)
cp_grid <- expand.grid(.cp=seq(0.001, 0.3, 0.0001))
train(ANSWERED ~. , data=train, method="rpart", trControl= num_fold, tuneGrid=cp_grid)
cv_tree <- rpart(ANSWERED ~., data = train, method = "class", cp= )
prp(cv_tree)
pred_cv <- predict(cv_tree, newdata = test, type="class")
table(test$ANSWERED, pred_cv)



  #------lasso regression------------


pre_proc_val <- preProcess(train, method = c("center", "scale"))
train = predict(pre_proc_val, train)
test = predict(pre_proc_val, test)
summary(train)


x <-data.matrix(subset(train, select=-c(ANSWERED)))
y <- train$ANSWERED

testX <- data.matrix(subset(test, select=-c(ANSWERED)))
testY <- test$ANSWERED

cv.lasso <-  cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")

plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se

as.matrix(coef(cv.lasso, s="lambda.min"))
as.matrix(coef(cv.lasso, s="lambda.1se"))

model1 <- glmnet(x, y, alpha =1, lambda = cv.lasso$lambda.min, family = "binomial")
model2 <- glmnet(x, y, alpha =1, lambda = cv.lasso$lambda.1se, family = "binomial")

pred1_test <- predict(model1, newx = testX, s = "lambda.min", type = "class")
pred2_test <- predict(model2, newx = testX, s = "lambda.1se", type = "class")

compare <- cbind(pred1_test, pred2.test, testY)
colnames(compare) <- c("pred1", "pred2", "y.test")

#plot(compare[,c(1,3)],col="blue")
#abline(0,1)

#plot(compare[,c(2,3)],col="red")
#abline(0,1)

table(testY, pred1_test)

rocs <- roc.glmnet(pred1_test, newy = testY)


plot(rocs)



pred_lasso <- prediction(pred1_test,as.numeric(testY))
perf_lasso <- performance(pred_lasso, "tpr", "fpr")
plot(perf_lasso)
as.numeric(performance(pred_lasso, "auc")@y.values)


# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred1_test,"tpr","fpr")
performance(pred1_test,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


#---------------------------------Train data set based on y= product------------------------------------------------------------------------------------

#-----------------test and tran dataset---------------
set.seed(2)
split <- sample.split(Products_excludeAnswer$PRODUCT, SplitRatio = 0.9)
train <- subset(Products_excludeAnswer, split==T)
test <- subset(Products_excludeAnswer, split==F)


#-----------------classification on y = product---------------
t_model <- rpart(PRODUCT ~. , data=train, method = "class", minbucket=5)
prp(t_model)

t_pred <- predict(t_model, newdata = test , type="class" )
table(test$PRODUCT, t_pred)


#m <- as.data.frame(t_pred)
#test$ANSWERED

pred_tree <- prediction(as.numeric(t_pred), test$PRODUCT)
perf_tree <- performance(pred_tree, "tpr", "fpr")
plot(perf_tree)
as.numeric(performance(pred_tree, "auc")@y.values)

precrec_obj <- evalmod(scores = as.numeric(t_pred), test$ANSWERED, mode="basic")
autoplot(precrec_obj)


