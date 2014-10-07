setwd("~/Documents/GitHub/BikeSharingDemand/R")
train <- read.csv("../csv/train.csv")
test <- read.csv("../csv/test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Join together the test and combi sets for easier feature engineering
test$count <- NA
test$casual <- NA
test$registered <- NA
combi <- rbind(train, test)

#this is a naive feature engineering, I think the ranges should be changed
#TODO: play around with ranges
combi$temprange <- '0-10'
combi$temprange[combi$temp > 10 & combi$temp <= 20] <- '10-20'
combi$temprange[combi$temp > 20 & combi$temp <= 30] <- '20-30'
combi$temprange[combi$temp > 30] <- '30+'

combi$atemprange <- '0-10'
combi$atemprange[combi$atemp > 10 & combi$atemp <= 20] <- '10-20'
combi$atemprange[combi$atemp > 20 & combi$atemp <= 30] <- '20-30'
combi$atemprange[combi$atemp > 30 & combi$atemp <= 40] <- '30-40'
combi$atemprange[combi$atemp > 40] <- '40+'

combi$humidityrange <- '0-25'
combi$humidityrange[combi$humidity > 25 & combi$humidity <= 50] <- '25-50'
combi$humidityrange[combi$humidity > 50 & combi$humidity <= 75] <- '50-75'
combi$humidityrange[combi$humidity > 75 & combi$humidity <= 100] <- '75-100'

combi$windspeedrange <- '0-7'
combi$windspeedrange[combi$windspeed > 7 & combi$windspeed <= 10] <- '7-10'
combi$windspeedrange[combi$windspeed > 10 & combi$windspeed <= 13] <- '10-13'
combi$windspeedrange[combi$windspeed > 13 & combi$windspeed <= 16] <- '13-16'
combi$windspeedrange[combi$windspeed > 16 & combi$windspeed <= 19] <- '16-19'
combi$windspeedrange[combi$windspeed > 19 & combi$windspeed <= 30] <- '19-30'
combi$windspeedrange[combi$windspeed > 30] <- '30+'

combi$datetime <- as.character(combi$datetime)
combi$hour <- sapply(combi$datetime, FUN=function(x) {strsplit(x, split='[ .]')[[1]][2]})
combi$hour <- sapply(combi$hour, FUN=function(x) {strsplit(x, split='[:.]')[[1]][1]})

combi$hourrange <- '0-7'
combi$hourrange[combi$hour > 7 & combi$hour <= 12] <- '7-12'
combi$hourrange[combi$hour > 12 & combi$hour <= 16] <- '12-16'
combi$hourrange[combi$hour > 16 & combi$hour <= 20] <- '16-20'
combi$hourrange[combi$hour > 20] <- '20+'

combi$hourrange <- factor(combi$hourrange)
combi$temprange <- factor(combi$temprange)
combi$atemprange <- factor(combi$atemprange)
combi$humidityrange <- factor(combi$humidityrange)
combi$windspeedrange <- factor(combi$windspeedrange)

train <- combi[1:10886,]
test <- combi[10887:17379,]

# Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(count) ~ hourrange + season + holiday + workingday + weather + temprange + atemprange + windspeedrange + humidityrange,
                    data=train, importance=TRUE, ntree=500)

# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)