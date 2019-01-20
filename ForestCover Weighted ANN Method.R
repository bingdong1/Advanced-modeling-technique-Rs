rm(list=ls()) 

.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")

require(data.table) 
require(plyr)  
require(dplyr) 
require(lubridate) 
require(reshape2)
require(ggplot2)
require(ggthemes)
require(scales)
require(GGally)
require(gridExtra)
require(nnet)
require(pander)
require(captioner)

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

# confirm equal sampling in training set for each cover type
table(data.train$Soil_Type15)

names(data.train)

levels(data.train$soilType)

#####################################
#Train_data Format
#####################################

#Remove Character variable Train
data.train$train <- NULL

load("U:/midterm//best.ann2.RData")

#Recode the Lables for the "Wilder_Area" to one column. 
newLabels <- c("Rawah","Neota","ComanchePeak","CachePoudre")
oldCols <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")

for(i in 1:length(newLabels)) {
  refColumn<-oldCols[i]
  refValue<-newLabels[i]
  data.train<-data.train[get(refColumn)==1,wildernessArea:=refValue]
}

#Recode the Lables for the "SoilType" to one column. 
newLabels<-c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
             "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
             "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
             "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
             "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

oldCols <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
             "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
             "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
             "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
             "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

for(i in 1:length(newLabels)) {
  refColumn<-oldCols[i]
  refValue<-newLabels[i]
  data.train<-data.train[get(refColumn)==1,soilType:=refValue]
}

table(data.train$soilType)

#remove the binary columns
data.train <- data.train[ , colnames(data.train[,11:54,with=FALSE]):=NULL]

#reorder the columns
colOrder<-c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Horizontal_Distance_To_Fire_Points","Hillshade_9am ","Hillshade_Noon","Hillshade_3pm","wildernessArea","soilType","Cover_Type")  

setcolorder(data.train, colOrder)

#Shorten names for readability.
newNames<-c("Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire","HS.9am","HS.noon","HS.3pm","wildernessArea","soilType","coverType")
setnames(data.train,colOrder,newNames)

table(data.train$soilType)

#Convert cateogry variables to factors

data.train$soilType = as.factor(data.train$soilType)
data.train$wildernessArea = as.factor(data.train$wildernessArea)


#####################################
#Test_data Format
#####################################

#Remove Character variable Train
data.test$train <- NULL

#Recode the Lables for the "Wilder_Area" to one column. 
newLabels <- c("Rawah","Neota","ComanchePeak","CachePoudre")
oldCols <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")

for(i in 1:length(newLabels)) {
  refColumn<-oldCols[i]
  refValue<-newLabels[i]
  data.test<-data.test[get(refColumn)==1,wildernessArea:=refValue]
}

data.test$Soil_Type7 <- NULL
data.test$Soil_Type8 <- NULL
data.test$Soil_Type15 <- NULL


#Recode the Lables for the "SoilType" to one column. 
newLabels<-c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6",
             "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type16",
             "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
             "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
             "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

oldCols <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6",
             "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type16",
             "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
             "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
             "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

for(i in 1:length(newLabels)) {
  refColumn<-oldCols[i]
  refValue<-newLabels[i]
  data.test<-data.test[get(refColumn)==1,soilType:=refValue]
}

str(data.test)

#remove the binary columns
data.test <- data.test[ , colnames(data.test[,11:51,with=FALSE]):=NULL]

#reorder the columns
colOrder<-c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Horizontal_Distance_To_Fire_Points","Hillshade_9am ","Hillshade_Noon","Hillshade_3pm","wildernessArea","soilType","Cover_Type")  

setcolorder(data.test, colOrder)

#Shorten names for readability.
newNames<-c("Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire","HS.9am","HS.noon","HS.3pm","wildernessArea","soilType","coverType")
setnames(data.test,colOrder,newNames)

#Convert cateogry variables to factors

data.test$soilType = as.factor(data.test$soilType)
data.test$wildernessArea = as.factor(data.test$wildernessArea)


#Creating Accuracy Function

weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

#Normalized the train data
normed <- cbind(as.data.frame(lapply(data.train[,-c(11,12,13)], function(col) { col / max(abs(col)) })),data.train[,11:13])

#Normalized the test data
normedTest <- cbind(as.data.frame(lapply(data.test[,-c(11,12,13)], function(col) { col / max(abs(col)) })),data.test[,11:13])


#load("U:/R Training//best.ann.RData")

#predAnn <- predict(best.ann, normed[,-13], type="class")

#best model
#Train Accuracy
# weighted.acc(predAnn,normed[,13]) #0.8000401   
# table(predAnn,normed[train_ind,13])

#Weighted ANN Model

#Set the weight class
cw1 <- rep(1, 7) # all equal
cw2 <- c(10, 100, 100, 10, 1, 10, 1) # powers of 10
freqs <- as.data.frame(table(normed$coverType))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(normed[,1])/as.integer(s[2])})) # 1/counts

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("SpruceFir", "LodgepolePine", "PonderosaPine", "Cottonwood-Willow", "Aspen", "Douglas-fir", "Krummholz")

#Normalized the data
#normed <- cbind(as.data.frame(lapply(coverData1[,-c(11,12,13)], function(col) { col / max(abs(col)) })),coverData1[,11:13])

#Take sample of the original dataset since it take too long to run this model, over 8 hours for the full dataset

# sampleIndex <- createDataPartition(normed$coverType, p = 1,list = FALSE,times = 1)
# 
# normed1 <- normed[ sampleIndex,]
# 
# coverData1 <- coverData1[ sampleIndex,]


require('caret')
require('randomForest')


## 70% of the sample size

smp_size <- floor(1 * nrow(normed))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(normed)), size = smp_size)

train <- normed[train_ind, ]
#test <- normed1[-train_ind, ]

fix(train_ind)

#Check the train and test to make sure it has all covertype
#table(test$coverType)
#table(test$coverType)

results <- matrix(ncol=6, nrow=0) 
models <- list()

#memory.limit(10000) 

# Do not run for 3 interations, 1 iteration takes over 10 hours 
# Consider taking another sample

tm_start<-Sys.time()
for (i in 1:1)
{
  for (c in 1:length(class.weights[,1]))
  {
    data.weights <- do.call(rbind, Map(function(s)
    {
      class.weights[c,s]
    }, normed$coverType))
    
    for (h in 2:16)
    {
      cat("Run", i, "for c", c, "and h", h, "\n")
      
      # With normalised data (no need for range now)
      ann <- nnet(normed$coverType ~ ., data=normed[,-13], weights=data.weights, size=h, decay=5e-4, maxit=200)
      pred <- predict(ann, normed[,-13], type="class")
      pred1 <- predict(ann, normedTest[,-13], type="class")
      tacc <- weighted.acc(pred, normed[,13])
      wacc <- weighted.acc(pred1, normedTest[,13])
      #pacc <- sum(pred[-train_ind]==normed1[-train_ind,2])/length(pred[-train_ind])
      
      results <- rbind(results, c(h, tacc, wacc, 1, c, 2))
      models[[(length(models)+1)]] <- ann
      
      # With neither range nor normalisation
      #       ann <- nnet(train$coverType ~ ., data=coverData1[train_ind,-13], weights=data.weights[train_ind], size=h, decay=5e-4, maxit=200)
      #       pred <- predict(ann, coverData1[,-13], type="class")
      #       tacc <- weighted.acc(pred[train_ind], coverData1[train_ind,13])
      #       wacc <- weighted.acc(pred[-train_ind], coverData1[-train_ind,13])
      #       pacc <- sum(pred[-train_ind]==coverData1[-train_ind,2])/length(pred[-train_ind])
      #       
      #       results <- rbind(results, c(h, tacc, wacc, pacc, c, 3))
      #       models[[(length(models)+1)]] <- ann
      
    }
  }
}
tm_start
Sys.time()

# Visualise results
cols <- do.call(rbind, Map(function(c)
{
  if (c==1) "green" 
  else if (c == 2) "blue" 
  else if (c == 3) "red" 
  else "black"
}, results[,5]))

pts <- do.call(rbind, Map(function(v)
{
  if (v==1) "r" # range
  else if (v == 2) "n" # normalised input
  else if (v == 3) "p" # 
  else "x"
}, results[,6]))

plot(results[,3] ~ results[,3], ylim=c(0,1), col=cols, pch=pts, xlab="Hidden neurons", ylab="Weighted accuracy")

best.row <- match(max(results[,3]), results[,3]) #Row 125
best.ann <- models[[best.row]]


pred <- predict(best.ann, normed[,-13], type="class")

#best model
#Train Accuracy
weighted.acc(pred,normed[,13]) #0.8000401  

table(pred,normed[train_ind,13])



