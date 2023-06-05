# Mahsa Choopannejad / student number: 98207477 / Exam / Data-Driven decision-making class - Spring 1401



#-------------------importing libraries---------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ISLR)
library(lubridate)
library(ggrepel)
library(tidyverse)
library(dslabs)
#-------------------Importing Data------------

h <- read.csv("City.csv" ,stringsAsFactors = T)
summary(h)
View(h)
str(h)

#------------------Data Cleaning------------------
  #all data based on summary semms valid so no cleaning step

#-------------------Visualization-----------------------

#----------Question #1 : plot Price median in each neubour

plot1 <- h %>% 
  ggplot(mapping= aes(x=h$LON, y=h$LAT)) +
  geom_point(aes(color=h$MEDV))+
  geom_text(aes(label=h$TRACT), size=1.5,nudge_y =0.005,   check_overlap = T) +
  ggtitle("Median of price in each neibour")
plot1



#----------Question #2 : Logistic regression--------------- 

h$Value <- ifelse(h$MEDV >22.69 ,"High","Low")
h$Value <- as.factor(h$Value)

#-------------------Descriptive analysis-----------------------


  #-------------------Visualization-----------------------

    #------- filtering plot 1 to insight-------    NOT complete

plot2 <- h %>% filter(ZN>12) %>%  
  ggplot(mapping= aes(x=h$DIS)) +
  geom_point(aes(color=h$MEDV))+
  geom_text(aes(label=h$TRACT), size=3,nudge_y =0.5,   check_overlap = T) +
  ggtitle("Median price in each neibour")
plot2


  #---- facet grid on Median of price---------

ggplot(data = h , aes(x = h$INDUS , fill = h$RAD)) +
  geom_bar() +
  ggtitle ("Value of real states based on industry ratio and reach to highway") +
  labs (fill = "Rad: Reach to Highway, C is best and A is worst") + 
  facet_grid(.~h$Value)


#-------------------Predictive analysis-----------------------

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

h_forPrediction <- subset(h, select=-c(TRACT, MEDV))

    #-----------------correlation analysis---------------

model.matrix(~0+., data=h_forPrediction) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F,
             type="lower",
             lab=TRUE,
             lab_size=2,
             #p.mat=p.mat,
             outline.color = "white",
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"))

#---------------------------------Train data set based on y= answered------------------------------------------------------------------------------------
h <- read.csv("City.csv" ,stringsAsFactors = T)
h$Value <- ifelse(h$MEDV >22.69 ,"High","Low")
h$Value <- as.factor(h$Value)
h_forPrediction <- subset(h, select=-c(TRACT, MEDV))

#-----------------test and train dataset---------------
set.seed(2)
split <- sample.split(h_forPrediction$Value, SplitRatio = 0.7)
train1 <- subset(h_forPrediction, split==T)
test <- subset(h_forPrediction, split==F)

#------------logistic model------------

log_model <- glm(Value ~. , data=train1, family = "binomial")
summary(log_model)

  #------ drop correlated columns from train1
train <- subset(train1, select=-c(LON, LAT, CRIM, ZN, INDUS, RAD, NOX, RM))


log_model <- glm(Value ~. , data=train, family = "binomial")
summary(log_model)



log_pred <- predict(log_model, newdata = test, type = "response")
table(test$Value, log_pred>0.5)

pred_log <- prediction(log_pred, as.numeric(test$Value))
perf_log <- performance(pred_log, "tpr", "fpr")
plot(perf_log)
as.numeric(performance(pred_log, "auc")@y.values)

Value_of_logistic_prediction<- log_pred

test <- test %>% mutate(Value_of_logistic_prediction)
summary(test)
str(test)


#----------Question #3 : Linear regression--------------- 

mod1 <- lm(h$MEDV~., data=h)
summary(mod1)

h_forLinearRegreesion <- subset(h, select=-c(TRACT))

mod2 <- lm(h_forLinearRegreesion$MEDV~., data=h_forLinearRegreesion)
summary(mod2)


#----------Question #3 : Decision tree--------------- 
h <- read.csv("City.csv" ,stringsAsFactors = T)


h_forDecisiontree <- subset(h, select=-c(TRACT))

#-----------------test and train dataset---------------
set.seed(2)
split <- sample.split(h_forDecisiontree$MEDV, SplitRatio = 0.7)
train_tree <- subset(h_forDecisiontree, split==T)
test_tree <- subset(h_forDecisiontree, split==F)


t_model <- rpart(MEDV ~. , data=train_tree, minbucket=5)

prp(t_model)

t_pred <- predict(t_model, newdata = test_tree )
table(test_tree$MEDV, t_pred)

t_RSS <- sum((t_pred-test_tree$MEDV)^2)
t_RSS


#-------- question #3 for choose data as test data-----------



choose <- read.csv("Choose.csv" ,stringsAsFactors = T)
Choose_test<- subset(choose, select=-c(TRACT))

#-----------------test and train dataset for tree---------------



t_model <- rpart(MEDV ~. , data=train_tree, minbucket=5)

prp(t_model)

t_pred <- predict(t_model, newdata = Choose_test)
tree_predicion<- t_pred

Choose_test <- Choose_test %>% mutate(tree_predicion)

Choose_test$Profit <- ((Choose_test$MEDV)-(Choose_test$tree_predicion))/(Choose_test$MEDV)
Choose_test$Profit_absolute <- ((Choose_test$MEDV)-(Choose_test$tree_predicion))


                  

#-------------------Prescriptive analysis-----------------------
