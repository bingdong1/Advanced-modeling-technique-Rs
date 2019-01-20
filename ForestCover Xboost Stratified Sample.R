rm(list=ls()) 

.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(data.table)
library(FeatureHashing)
library(Matrix)
require(randomForest)
require(dplyr)
require(ggplot2)
library(pROC)
library(stringr)
library(dummies)
library(Metrics)
library(kernlab)
library(mlbench)
library(Ckmeans.1d.dp)
library(DiagrammeR)

coverData<-data.table::fread(file="U:/Midterm/covtype.data",header=FALSE,na.strings=c(""))
key <- read.delim(file="U:/Midterm/forestcover_key.txt")   

covtype.names <- row.names(key)
covtype.names.complete <- c() 

for (i in 1:ncol(coverData)) {
  if (i <= 10) {
    covtype.names.complete[i] <- covtype.names[i]
  }
  else if (i > 10 & i <= 14) {
    covtype.names.complete[i] <- paste0("Wilderness_Area",i-10) 
  }
  else if (i > 14 & i <= 54) {
    covtype.names.complete[i] <- paste0("Soil_Type",i-14)
  }
  else if (i == 55) {
    covtype.names.complete[i] <- "Cover_Type"
  }
}
colnames(coverData) <- covtype.names.complete

library(plyr)

#Change Cover type to actual values 
coverData$Cover_Type <- mapvalues(coverData$Cover_Type, from = c(1,2,3,4,5,6,7), 
                                  to = c("1.SpruceFir","2.LodgepolePine","3.PonderosaPine",
                                         "4.CottonwoodWillow","5.Aspen","6.Douglasfir","7.Krummholz"))


# set numerical variables as factors for soil type, wilderness area, and forest cover
coverData$Cover_Type <- as.factor(coverData$Cover_Type)

# set the number of records that should be included from each category for the training group
train.size <- 1620

# create a vector of the row numbers that correspond to each cover type
coverData.train.sample.case <- c()
for(i in 1:nlevels(coverData$Cover_Type)) {
  coverData.train.sample.case <- c(coverData.train.sample.case,which(coverData$Cover_Type==levels(coverData$Cover_Type)[i]))
}

# create a vector of the random index that will be assigned to each record of each cover type
coverData.train.sample.index <- c()
set.seed(1234)
for(i in 1:nlevels(coverData$Cover_Type)) {
  coverData.train.sample.index <- c(coverData.train.sample.index,sample(1:summary(coverData$Cover_Type)[i], summary(coverData$Cover_Type)[i], replace = FALSE))
}

# create a data frame of the record row number and the sample index number
coverData.train.sample <- cbind(case = coverData.train.sample.case, index = coverData.train.sample.index)

# create a data frame of the number of rows in the data set and merge it with the sampling assignment
# data frame so that it is in the same order as the original data frame
coverData.rows <- data.frame(case = c(1:nrow(coverData)))
coverData.train.sample.new <- merge(coverData.rows, coverData.train.sample, by = "case")

# assign a train vs. test variable in the original dataset
coverData$train <- NA
coverData$train <- ifelse(coverData.train.sample.new$index <= train.size, "train", "test")

# create new data frames for the training and test datasets
data.train <- coverData[coverData$train=="train",]
data.test <- coverData[coverData$train=="test",]

table(coverData$Cover_Type)
table(data.train$Cover_Type)
table(data.test$Cover_Type)

data.test$Cover_Type1 <- as.integer(data.test$Cover_Type)

data.test$Cover_Type <- NULL
data.test$train <- NULL
data.test$Cover_Type <- data.test$Cover_Type1
data.test$Cover_Type1 <- NULL



data.train$Cover_Type1 <- as.integer(data.train$Cover_Type)

data.train$Cover_Type <- NULL
data.train$train <- NULL
data.train$Cover_Type <- data.train$Cover_Type1
data.train$Cover_Type1 <- NULL



#Create response variable for train and test
y<- data.train$Cover_Type
z<- data.test$Cover_Type

data.test$Cover_Type <- NULL

data.train$Cover_Type <- NULL

load("G:/Employee Folders/Dong Bing//XboostFit.RData")

testMatrix <- data.test[,lapply(.SD,as.numeric)] %>% as.matrix

trainMatrix <- data.train[,lapply(.SD,as.numeric)] %>% as.matrix

#prediction for train
y_pred <- predict(bst, trainMatrix)

#Confustion Matrix train
confusionMatrix(y_pred,y) #0.9526


#prediction for test
y_pred_test <- predict(bst, testMatrix)

#Confustion Matrix test
confusionMatrix(y_pred_test,z) #0.9525
