install.packages("rpart")
installed.packages("rpart.plot")

hitter <- read.csv("Hitters2.csv", stringsAsFactors = T)
str(hitter)
hitter <- na.omit(hitter)

hist(hitter$Salary)
hitter$Salary <- log(hitter$Salary)

library(caTools)
set.seed(1)
split <- sample.split(hitter$Salary, SplitRatio = 0.8)
train <- hitter[split, ]
test <- hitter[!split, ]
l_model <- lm(Salary ~. , data=train)
summary(l_model)

library(rpart)
library(rpart.plot)

t_model <- rpart(Salary ~. , data=train, minbucket=30)
prp(t_model)

l_pred <- predict(l_model, newdata = test)
l_RSS <- sum((l_pred-test$Salary)^2)

t_pred <- predict(t_model, newdata = test)
t_RSS <- sum((t_pred- test$Salary)^2)
