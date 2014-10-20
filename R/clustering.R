#set the repertory and read the file train (to be used into the clustering process)
getwd()
dir()
setwd("C:/Users/User/Documents/KaggleProject/BikeSharing")

train<-read.csv(file.choose())
str(train)

#use mean as baseline for standard prediction. (will help to evaluate the performance of other method)

baseline<-c(mean(train$casual),mean(train$registered),mean(train$count))

#split the date format into component

install.packages("lubridate")
library(lubridate)
splitdate<-ymd_hms(train$datetime)
train$day<-wday(splitdate, label=TRUE)
train$hour<-hour(splitdate)
train$month<-month(splitdate)


# Main Objectif: segment the original data set into homogeneous group of renter

# factors extraction: format data frame in order to keep relevant factors for clustering

# 1. CART to detect the trend of the factors and determine the more relevant factors affecting each var. (casual and registered)
# CART is usefull as it don't required the linearity of the underlying data

# 2. Use the clustering methods (Hierarichal & Kmeans) to split the data into group

trainC<-train
trainC$datetime<-NULL

# download packages for CART

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# cart for casual with graphic

Casual<-trainC
Casual$registered<-NULL
Casual$count<-NULL
str(Casual)
CartCasual<-rpart(casual~.,data=Casual)
prp(CartCasual)

# cart for registered with graphic

Reg<-trainC
Reg$casual<-NULL
Reg$count<-NULL
str(Reg)
CartReg<-rpart(registered~.,data=Reg)
prp(CartReg)

# normalize the data for clustering

install.packages("caret")
library(caret)

trainCl<-trainC
trainCl$day<-as.numeric(trainCl$day)
Prepro<-preProcess(trainCl)
normtrainc<-predict(Prepro,trainCl)
str(normtrainc)

# Clustering Graph: dendogram

distance<-dist(normtrainc, method="euclidean")
Genclustering<-hclust(distance,method="ward.D")
plot(Genclustering)

# appropriate number of cluster > 8

Hclusters<-cutree(Genclustering, k=8)

# split original data set according to H clustring group
# Maxime is it possible to create a for loop to allocate them automatically?

HC1<-subset(trainC,Hclusters == 1)
HC2<-subset(trainC,Hclusters == 2)
HC3<-subset(trainC,Hclusters == 3)
HC4<-subset(trainC,Hclusters == 4)
HC5<-subset(trainC,Hclusters == 5)
HC6<-subset(trainC,Hclusters == 6)
HC7<-subset(trainC,Hclusters == 7)
HC8<-subset(trainC,Hclusters == 8)

# analyse the H clustering based on the variable to define type of  profile

tapply(trainC$casual,Hclusters,mean)


