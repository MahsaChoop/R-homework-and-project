

# Mahsa Choopannejad / student number: 98207477 / Exercise #1 / Data-Driven decision-making class - Spring 1401


#-------------------importing libraries---------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ISLR)
library(lubridate)
library(ggrepel)
library(tidyverse)
#-------------------Importing Data------------

h <- read.csv("no-show.csv" ,stringsAsFactors = F)
summary(h)
View(h)
str(h)

h$Gender <- as.factor(h$Gender)
h$Scholarship <- as.factor(h$Scholarship)
h$Hypertension <- as.factor(h$Hypertension)
h$Diabetes <- as.factor(h$Diabetes)
h$Alcoholism <- as.factor(h$Alcoholism)
h$Handicap <- as.factor(h$Handicap)
h$SMS_received <- as.factor(h$SMS_received)
h$No.show <- as.factor(h$No.show)

summary(h)
str(h)

#------------------Data Cleaning------------------

invalid_age <- subset(h, h$Age<0)
invalid_age
h <- subset(h, h$Age>0)
summary(h)

n_occur <- data.frame(table(h$AppointmentID))
n_occur[n_occur$Freq > 1,]
sum_of_obsrv <- 106987
#-------------------Visualization-----------------------

ggplot(data = h, aes( x =factor(1), fill = Gender))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on gender")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = guide_legend(title = "Group")) +
  theme_minimal()
  
  

ggplot(data = h, aes( x =factor(1), fill = Scholarship))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on Scholarship")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel2")+
  guides(fill = guide_legend(title = "Scholarship")) +
  theme_minimal()

ggplot(data = h, aes( x =factor(1), fill = Hypertension))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on Hypertension")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel3")+
  guides(fill = guide_legend(title = "Hypertension")) +
  theme_minimal()

ggplot(data = h, aes( x =factor(1), fill = Diabetes))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on Diabetes")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = guide_legend(title = "Diabetes")) +
  theme_minimal()

ggplot(data = h, aes( x =factor(1), fill = Alcoholism))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on Alcoholism")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = guide_legend(title = "Alcoholism")) +
  theme_minimal()

ggplot(data = h, aes( x =factor(1), fill = Handicap))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on Handicap")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel2")+
  guides(fill = guide_legend(title = "Handicap")) +
  theme_minimal()

ggplot(data = h, aes( x =factor(1), fill = SMS_received))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on SMS_received")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel3")+
  guides(fill = guide_legend(title = "SMS_received")) +
  theme_minimal()

ggplot(data = h, aes( x =factor(1), fill = No.show))+
  geom_bar(width =1)+
  coord_polar(theta = "y")+
  ylab("Patients based on No.show")+
  xlab("Pi chart")+
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = guide_legend(title = "No.show")) +
  theme_minimal()


#---------No show details in facet ----------------

ggplot(data = h , aes(x = Scholarship , fill = Gender)) +
  geom_bar() +
  ggtitle ("No show in each Gender and given scholarship") +
  labs (fill = "Gender") + 
  facet_grid(.~No.show)

ggplot(data = h , aes(x = Scholarship , fill = No.show)) +
  geom_bar() +
  ggtitle ("No show in given scholarship and SMS_received") +
  labs (fill = "No show") + 
  facet_grid(.~SMS_received)

ggplot(data = h , aes(x = Age , fill = No.show)) +
  geom_bar() +
  ggtitle ("No show in Age and hypertension") +
  labs (fill = "No show") + 
  facet_grid(.~Hypertension)

ggplot(data = h , aes(x = Age , fill = No.show)) +
  geom_bar() +
  ggtitle ("No show in Age and Diabetes") +
  labs (fill = "No show") + 
  facet_grid(.~Diabetes)

ggplot(data = h , aes(x = Age , fill = No.show)) +
  geom_bar() +
  ggtitle ("No show in Age and Alcoholism") +
  labs (fill = "No show") + 
  facet_grid(.~Alcoholism)
  
ggplot(data = h , aes (y = Age , x = No.show))+
  geom_boxplot() +
  xlab ("No.show")

neibourhoods <- table(h$Neighbourhood, h$No.show)
neibourhoods <- as.data.frame.matrix(neibourhoods)
neibourhoods$Title <- rownames(neibourhoods)
neibourhoods$NoShow_percentage <- (neibourhoods$Yes)/(neibourhoods$Yes+neibourhoods$No)*100

ggplot(data = neibourhoods , aes(x = reorder(Title, NoShow_percentage)  , y = NoShow_percentage)) +
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()

#---------same day reservation -------------------------

h <- mutate(h, reservation_period = difftime(ymd_hms(h$AppointmentDay), ymd_hms(h$ScheduledDay), units = "hours"))

h$reserveDate_vs_AppointmentDate <- ifelse(h$reservation_period <24 ,"Same day reservation","Not Same day reservation")
h$reserveDate_vs_AppointmentDate <- as.factor(h$reserveDate_vs_AppointmentDate)


ggplot(data = h, aes( x =factor(1), fill = reserveDate_vs_AppointmentDate))+
  geom_bar(width =1)+
  coord_polar(theta = "y")

ggplot(data = h , aes(x = reserveDate_vs_AppointmentDate, fill = No.show)) +
  geom_bar() 

summary(h)

ggplot(data = h , aes (y = reservation_period , x = No.show))+
  geom_boxplot() +
  xlab ("No.show")


#---------weekday in no show -----------------------
h <- mutate(h, weekday = wday(h$AppointmentDay))

ggplot(data=h, aes(x = h$weekday, fill = No.show)) +
  geom_bar()

#--------tests--------------------------------------

kzSMS <- table(h$SMS_received, h$No.show)
kzSMS
resultSMS <- chisq.test(kzSMS)
resultSMS

fit1 <- glm(as.factor(No.show) ~ SMS_received, data=h, family="binomial")
summary(fit1)

kzScholarship <- table(h$Scholarship, h$No.show)
kzScholarship
result <- chisq.test(kzScholarship)
result

fit1 <- glm(Scholarship~ No.show , data=h, family="binomial")
summary(fit1)

kzAlcoholism <- table(h$Alcoholism, h$No.show)
kzAlcoholism
result <- chisq.test(kzAlcoholism)
result

kzHypertension <- table(h$Hypertension, h$No.show)
kzHypertension
result <- chisq.test(kzHypertension)
result

kzDiabetes <- table(h$Diabetes, h$No.show)
kzDiabetes
result <- chisq.test(kzDiabetes)
result

kzHandicap <- table(h$Handicap, h$No.show)
kzHandicap
result <- chisq.test(kzHandicap)
result

fit1 <- glm(No.show ~ Hypertension, data=h, family="binomial")
summary(fit1)







