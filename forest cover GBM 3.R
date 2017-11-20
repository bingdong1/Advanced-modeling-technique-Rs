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

#load("U:/midterm//best.ann2.RData")

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


table(data.train$soilType)
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


# library(caret)
# 
# #GBM Model
# 
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 5, 
#   ## repeated 5 times
#   repeats = 1, 
#   allowParallel = TRUE)
# 
# 
# gbmGrid <-  expand.grid(interaction.depth = c(25), #c(5:30), # 12 old; 25 was selected as optimal
#                         n.trees = c(1000), #20000, 30000, 40000, 50000), 
#                         shrinkage = c(0.01), #c(0.01, 0.005),
#                         n.minobsinnode = 7) #c(1, 3, 5, 7, 10))
# fix(gbmGrid)
# gbmFit <- train(coverType ~ ., data = data.train,
#                 method = "gbm",
#                 trControl = fitControl,
#                 preProc = c("center", "scale"),
#                 ## Now specify the exact models 
#                 ## to evaluate:
#                 tuneGrid = gbmGrid,
#                 maximize=FALSE)
# 
# trellis.par.set(caretTheme())
# plot(gbmFit)  


weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

load("G:/Employee Folders/Dong Bing//gbmFit3.RData")

#Train Data Prediction
gbm_pred <- predict(gbmFit, newdata = data.train, type="raw")

#Train Data Accuracy
weighted.acc(gbm_pred,data.train$coverType) #0.9850088 

confusionMatrix(gbm_pred, data.train$coverType) #0.985

#find column with NA, no missing value within this dataset
which(sapply(data.train,anyNA))

colSums(sapply(coverData, is.na))

#remove NA
row.has.na <- apply(data.test, 1, function(x){any(is.na(x))})

sum(row.has.na)

data.test1 <- data.test[!row.has.na,]

#Test Data Prediction

gbm_predTest <- predict(gbmFit, newdata = data.test1[,-13], type="raw")

#Test Data Accuracy
weighted.acc(gbm_predTest,data.test1$coverType) #0.8423738
#1000 0.8564225
#gbmFit 0.8423738
#gbmFit2 0.7698606
#gbmFit3 0.7354835

confusion <- confusionMatrix(gbm_predTest, data.test1$coverType)$overall[1] #0.7309

names(confusion)

confusion$overall[1]

#1000 0.7309
#gbmFit 0.7106
#gbmFit2 0.6169
#gbmFit3 0.5728

confusionMatrix

save(gbmFit, file=paste0("G:/Employee Folders/Dong Bing//gbmFit.RData"))

save(gbmFit, file=paste0("G:/Employee Folders/Dong Bing//gbmFit4_1000tree.RData"))


pred <- predict(best.ann, normedTest[,-13], type="class")




