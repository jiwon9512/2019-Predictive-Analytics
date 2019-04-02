##Predictive Analysis Assignment2 - Linear Regression for Bike Sharing Data
library(dplyr)
library(stats)
library(corrplot)
library(ggplot2)
library(corrgram)
library(tidyverse)
library(PerformanceAnalytics)
library(car)
library(psych)


######Part 1: Exploratory Data Analysis#######
bike <- read.csv("hour.csv")
summary(bike)
str(bike)
dim(bike)

#check missing value
sum(is.na(bike))
is.null(bike) ## Checking for null values

##histogram for each variable
ggplot(data = bike) +geom_bar(mapping = aes(x = season))+ggtitle("Distribution of Season")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Seasons")

ggplot(data = bike) +geom_bar(mapping = aes(x = holiday))+ggtitle("Distribution of Holiday")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Holiday")

ggplot(data = bike) +geom_bar(mapping = aes(x = workingday))+ggtitle("Distribution of Workingday")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Workingday")

ggplot(data = bike) +geom_bar(mapping = aes(x = weathersit))+ggtitle("Distribution of Weather Situation")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Weathersit")

ggplot(data = bike) +geom_bar(mapping = aes(x = temp))+ggtitle("Distribution of Temperature")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Temp")

ggplot(data = bike) +geom_bar(mapping = aes(x = atemp))+ggtitle("Distribution of Feeling Temperature")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("atemp")

ggplot(data = bike) +geom_bar(mapping = aes(x = hum))+ggtitle("Distribution of Humidity")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Humidity")

ggplot(data = bike) +geom_bar(mapping = aes(x = windspeed))+ggtitle("Distribution of Windspeed")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Windspeed")

ggplot(data = bike) +geom_bar(mapping = aes(x = casual))+ggtitle("Distribution of Casual Rents")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Casual Rents")

ggplot(data = bike) +geom_bar(mapping = aes(x = registered))+ggtitle("Distribution of Registered Rents")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Registered Rents")

ggplot(data = bike) +geom_bar(mapping = aes(x = cnt))+ggtitle("Distribution of Total Rents")+theme(plot.title = element_text(hjust = 0.5))+ylab("Frequency")+xlab("Total Rents")


#x variable vs. Y
boxplot(bike$cnt ~ bike$yr,
        data = bike,
        main = "Total Bike Rentals Vs Year",
        xlab = "Year",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$season,
        data = bike,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$mnth,
        data = bike,
        main = "Total Bike Rentals Vs Month",
        xlab = "Month",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$hr,
        data = bike,
        main = "Total Bike Rentals Vs Hour",
        xlab = "Hour",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$holiday,
        data = bike,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Holiday",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$weekday,
        data = bike,
        main = "Total Bike Rentals Vs Weekday",
        xlab = "Weekday",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$workingday,
        data = bike,
        main = "Total Bike Rentals Vs Workingday",
        xlab = "Workingday",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$weathersit,
        data = bike,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$temp,
        data = bike,
        main = "Total Bike Rentals Vs Temperature",
        xlab = "Temperature",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$atemp,
        data = bike,
        main = "Total Bike Rentals Vs Feeling Temperature",
        xlab = "Feeling Temperature",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$hum,
        data = bike,
        main = "Total Bike Rentals Vs Humidity",
        xlab = "Humidity",
        ylab = "Total Bike Rentals") 

boxplot(bike$cnt ~ bike$windspeed,
        data = bike,
        main = "Total Bike Rentals Vs Wind Speed",
        xlab = "Wind Speed",
        ylab = "Total Bike Rentals") 



hist(my_data$windspeed, main="Histogram for Wind Speed", 
     xlab="wind speed", col = "red")


#correlation
#numbers
bike_correlo<- bike%>%
  select (season,yr,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp, hum, windspeed)
bike_correlo<- data.frame(bike_correlo)

bike_cplot <- cor(bike_correlo)
corrplot(bike_cplot, method="number")

#diagram
bike_cor <- cor(bike[3:14], method=c("spearman"))
bike_cor
corrplot(bike_cor)




######Part2: Building Regression model######
#Eliminate outlier
bike2 = bike[bike$dteday != '2011-03-10',]

#first model
bike_lm1 <- lm(cnt~instant+season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = bike2)
summary(bike_lm1)

##Continuous variables to factor variables 
bike2$season <- as.factor(bike2$season)
bike2$yr <- as.factor(bike2$yr)
bike2$mnth <- as.factor(bike2$mnth)
bike2$hr <- as.factor(bike2$hr)
bike2$holiday <- as.factor(bike2$holiday)
bike2$weekday <- as.factor(bike2$weekday)
bike2$workingday <- as.factor(bike2$workingday)
bike2$weathersit <- as.factor(bike2$weathersit)

#Second regression model
bike_lm2 <- lm(cnt~instant+season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = bike2)
summary(bike_lm2)

bike_lm21 <-lm(cnt~1, data= bike2)

#Variable Selection: Forward, Backward, Stepwise
bike_lm2forward <- step(bike_lm21,scope=list(lower=bike_lm21,upper=bike_lm2),direction = "forward")
summary(bike_lm2forward)

bike_lm2backward <- step(bike_lm2,scope=list(lower=bike_lm21,upper=bike_lm2),direction = "backward")
summary(bike_lm2backward)

bike_lm2both <- step(bike_lm21, scope = list(lower = bike_lm21, upper = bike_lm2), direction = "both")
summary(bike_lm2both)



#Third Regression model
bike_lm3 <- lm(cnt~season+yr+mnth+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed, data = bike2)
summary(bike_lm3)
par(mfrow = c(2,2))
plot(bike_lm3)
vif(bike_lm3)


#remove mnth and temp
bike_lm4 <- lm(cnt~season+yr+hr+holiday+weekday+weathersit+atemp+hum+windspeed, data = bike2)
summary(bike_lm4)

#log transformation
bike_lm5 <- lm(log(cnt)~season+yr+hr+holiday+weekday+weathersit+atemp+hum+windspeed, data = bike2)
summary(bike_lm5)
plot(bike_lm5)

#sqrt transformation
bike_lm6 <- lm(sqrt(cnt)~season+yr+hr+holiday+weekday+weathersit+atemp+hum+windspeed, data = bike2)
summary(bike_lm6)

#Á¦°ö transformation
bike_lm7 <- lm((cnt) ^2~season+yr+hr+holiday+weekday+weathersit+atemp+hum+windspeed, data = bike2)
summary(bike_lm7)



######Part 3: Confidence interval######
confint(bike_lm5, level=0.95)


######Part 4: ANOVA######
anova(bike_lm5)


######Part 5: Accuracy######
#training and test set
smp_size <- floor(0.8 * nrow(bike2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(bike2)), size = smp_size)

train <- bike2[train_ind, ]
test <- bike2[-train_ind, ]

bike_lm5 <- lm(log(cnt)~season+yr+hr+holiday+weekday+weathersit+atemp+hum+windspeed, data = train)
summary(bike_lm5)

predicted <- predict (bike_lm5, test)

#correlation
actuals_preds <- data.frame(cbind(actuals=log(test$cnt), predicteds=predicted))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy


#interval
predicted2 <- predict(bike_lm5, newdata = test, interval = "predict")
predicted2 <- as.data.frame(predicted2)
predicted2 <- cbind(predicted2, test$cnt)

help(predict)
tf <- NA
predicted2 <- cbind(predicted2, tf)
predicted2$'test$cnt' <- log(predicted2$'test$cnt')
predicted2$tf[predicted2$'test$cnt'>= predicted2$lwr & predicted2$'test$cnt' <= predicted2$upr] <- T
predicted2$tf[is.na(predicted2$tf)] <- F

sum(predicted2$tf=="TRUE")/dim(predicted2)[1] 
log(test$cnt)


#RMSE
bike_rmse <- sqrt(sum((exp(predicted)-log(test$cnt))^2)/length(test$cnt))
c(bike_rmse, rs = summary(bike_lm5)$rs)
par(mfrow=c(1,1))
plot(test$cnt, exp(predicted))

