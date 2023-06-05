library(tidyverse)
fr <- read.csv("framingham.csv")
str(fr)
summary(fr)
fr <- na.omit(fr)

library(caTools)
set.seed(222)
split <- sample.split(fr$TenYearCHD, SplitRatio = 0.7)
train <- subset(fr, split==T)
test <- subset(fr, split==F)

fit <- glm(TenYearCHD ~. , data= train, family="binomial")
summary(fit)
train %>% ggplot(aes(factor(diabetes), glucose)) + geom_boxplot()

library(ROCR)
train_predict <- predict(fit, type="response" )
table(train$TenYearCHD, train_predict>0.2)

tr_pred <- prediction(train_predict,train$TenYearCHD )
tr_perf <- performance(tr_pred, "tpr", "fpr")
plot(tr_perf, print.cutoffs.at=seq(0,1,0.1))


test_predict <- predict(fit, type="response", newdata = test )
table(test$TenYearCHD, test_predict>0.2)

ts_pred <- prediction(test_predict,test$TenYearCHD )
ts_perf <- performance(ts_pred, "tpr", "fpr")
plot(ts_perf, print.cutoffs.at=seq(0,1,0.1))

perf <- performance(ts_pred, "auc")
as.numeric(perf@y.values)
