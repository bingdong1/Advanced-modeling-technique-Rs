#rm(list=ls()) 

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

#Load Data
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

nameLastCol <- names(coverData)[ncol(coverData)]


#Create train and test data
set.seed(123)

sampleIndex <- createDataPartition(coverData$Cover_Type, p = 0.7,list = FALSE,times = 1)

data.train <- coverData[ sampleIndex,]

data.test <- coverData[-sampleIndex,]

#Create response variable for train and test
y<- data.train$Cover_Type
z<- data.test$Cover_Type

#Remove response variable from dataset
data.train$Cover_Type <- NULL
data.test$Cover_Type <- NULL

#Create Matrix for train and test data
trainMatrix <- data.train[,lapply(.SD,as.numeric)] %>% as.matrix
testMatrix <- data.test[,lapply(.SD,as.numeric)] %>% as.matrix

#Set up Model
numberOfClasses <- max(y) + 1

param <- list(colsample_bytree = .7,
              subsample = .7,
              booster = "gbtree",
              max_depth = 10,
              eta = 0.02,
              "objective" = "multi:softmax",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

#Exam cross validation error
# cv.nround <- 5
# cv.nfold <- 3
# 
# bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
#                 nfold = cv.nfold, nrounds = cv.nround)

#Model
nround = 1000
bst = xgboost(param=param, 
              data = trainMatrix, 
              label = y, 
              nrounds=nround,
              verbose = TRUE,
              print_every_n = 50,
              nthread = 2)

# save(bst, file=paste0("G:/Employee Folders/Dong Bing//XboostFit.RData"))

#Prediction for train
y_pred <- predict(bst, trainMatrix)

#prediction for test
y_pred_test <- predict(bst, testMatrix)

#Confusion Matrix train
confusionMatrix(y_pred,y) #0.9524

#Confustion Matrix test
confusionMatrix(y_pred_test,z) #0.9526


# Get the feature real names
# names <- dimnames(trainMatrix)[[2]]
# 
# xgb.plot.tree(feature_names = data.train$data@Dimnames[[2]], model = bst)


# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix)

