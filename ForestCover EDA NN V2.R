# title: "Team Checkpoint 2"
# subtitle: "Forest Cover Type Prediction"
# author: "Dong Bing, Richard Gunawarden, Kent Merdes, Christina Macholan, Tyler Wintermeyer
# EDA

#rm(list=ls()) 

####################
##Libraries required
####################

# memory.size(max=T)
# library(doParallel)
# library(foreach)
# #now, we have clearred our memory and loaded some performance libraries
# c1=makeCluster(3)
# registerDoParallel(c1)
# on.exit(stopCluster(cl))

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

##########################################################
#Read in the file, attach the column heading to the dataset
##########################################################  
  
  coverData1<-data.table::fread(file="U:/Midterm/covtype.data",header=FALSE,na.strings=c(""))
  key <- read.delim(file="U:/Midterm/forestcover_key.txt")   

  covtype.names <- row.names(key)
  covtype.names.complete <- c() 
  
  for (i in 1:ncol(coverData1)) {
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
  colnames(coverData1) <- covtype.names.complete


####################
#Data transformation
####################

library(plyr)
 
  #Change Cover type to actual values 
  coverData1$Cover_Type <- mapvalues(coverData1$Cover_Type, from = c(1,2,3,4,5,6,7), 
                                     to = c("1.SpruceFir","2.LodgepolePine","3.PonderosaPine",
                                            "4.Cottonwood-Willow","5.Aspen","6.Douglas-fir","7.Krummholz"))
  
  #Recode the Lables for the "Wilder_Area" to one column. 
  newLabels <- c("Rawah","Neota","ComanchePeak","CachePoudre")
  oldCols <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")

  for(i in 1:length(newLabels)) {
    refColumn<-oldCols[i]
    refValue<-newLabels[i]
    coverData1<-coverData1[get(refColumn)==1,wildernessArea:=refValue]
  }
  
  #Recode the Lables for the "SoilType" to one column. 
  newLabels<-c('Cathedral','Vanet','Haploborolis','Ratake','Vanet1','Vanet2','Gothic','Supervisor','Troutville','Bullwark1','Bullwark2','Legault','Catamount1','Pachic','unspecified','Cryaquolis','Gateview','Rogert','Typic1','Typic2','Typic3','Leighcan1','Leighcan2','Leighcan3','Leighcan4','Granile','Leighcan5','Leighcan6','Como1','Como2','Leighcan7','Catamount2','Leighcan8','Cryorthents','Cryumbrepts','Bross','Rock','Leighcan9','Moran1','Moran2')
  
  
  oldCols <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
               "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
               "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
               "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
               "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")
  
  for(i in 1:length(newLabels)) {
    refColumn<-oldCols[i]
    refValue<-newLabels[i]
    coverData1<-coverData1[get(refColumn)==1,soilType:=refValue]
  }
  
  #remove the binary columns
  coverData1 <- coverData1[ , colnames(coverData1[,11:54,with=FALSE]):=NULL]
  
  #reorder the columns
  colOrder<-c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Horizontal_Distance_To_Fire_Points","Hillshade_9am ","Hillshade_Noon","Hillshade_3pm","wildernessArea","soilType","Cover_Type")  
  
  setcolorder(coverData1, colOrder)
  
  #Shorten names for readability.
  newNames<-c("Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire","HS.9am","HS.noon","HS.3pm","wildernessArea","soilType","coverType")
  setnames(coverData1,colOrder,newNames)
  
  #Convert cateogry variables to factors
  
  coverData1$coverType = as.factor(coverData1$coverType)
  coverData1$soilType = as.factor(coverData1$soilType)
  coverData1$wildernessArea = as.factor(coverData1$wildernessArea)
 
#Base Model
  
#Creating Accuracy Function
  
weighted.acc <- function(predictions, actual)
{
    freqs <- as.data.frame(table(actual))
    tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
    tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
    acc.pc <- tab[,1]/freqs[,2]
    return(sum(acc.pc)/length(acc.pc))
}

#Random Forest Model
  
smp_size <- floor(0.70 * nrow(coverData1))

train_ind <- sample(seq_len(nrow(coverData1)), size = smp_size)

train <- coverData1[train_ind,]  
test <- coverData1[-train_ind,]

library(randomForest)
set.seed(711)
modelRF <- randomForest(coverType ~. ,
                          data = train, 
                          importance = TRUE, 
                          ntree = 200,
                          mtry = 3)

#Importance Plot
varImpPlot(modelRF)

#RF model summary
summary(modelRF)

#Prediction Training
mypredRFTrain=predict(modelRF, newdata=train)

#Prediction test
mypredRFTest=predict(modelRF, newdata=test)

#Training Accuracy
weighted.acc(mypredRFTrain,train$coverType)  #0.7309963

#Test Accuracy
weighted.acc(mypredRFTest,test$coverType)  #0.659445


#Base ANN Model

#normalize the data
normed <- cbind(train[,11:13], as.data.frame(lapply(train[,-c(11,12,13)], function(col) { col / max(abs(col)) })))

normedTest <- cbind(test[,11:13], as.data.frame(lapply(test[,-c(11,12,13)], function(col) { col / max(abs(col)) })))

#set timer for monitoring
tm_start<-Sys.time()
set.seed(123)
ann <- nnet(normed$coverType ~ ., data=normed[,-3], size=3, decay=5e-4, maxit=200)
tm_start
Sys.time()

#Prediction Training
mypredANNTrain <- predict(ann, normed[,-3], type = "class")

#Predction Testing
mypredANNTest <- predict(ann, normedTest[,-3], type = "class")

#Training Accuracy
weighted.acc(mypredANNTrain,normed$coverType)  #0.4180 not good

#Testing Accuracy
weighted.acc(mypredANNTest,normedTest$coverType)  #0.4166 not good


#Weighted ANN Model

#Set the weight class
cw1 <- rep(1, 7) # all equal
cw2 <- c(10, 100, 100, 10, 1, 10, 1) # powers of 10
freqs <- as.data.frame(table(normed$coverType))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(normed[,1])/as.integer(s[2])})) # 1/counts

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("SpruceFir", "LodgepolePine", "PonderosaPine", "Cottonwood-Willow", "Aspen", "Douglas-fir", "Krummholz")

#Normalized the data
normed <- cbind(as.data.frame(lapply(coverData1[,-c(11,12,13)], function(col) { col / max(abs(col)) })),coverData1[,11:13])

#Take sample of the original dataset since it take too long to run this model, over 8 hours for the full dataset

sampleIndex <- createDataPartition(normed$coverType, p = .1,list = FALSE,times = 1)

normed1 <- normed[ sampleIndex,]

coverData1 <- coverData1[ sampleIndex,]


# require('caret')
# require('randomForest')


## 70% of the sample size

smp_size <- floor(0.70 * nrow(normed1))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(normed1)), size = smp_size)

train <- normed1[train_ind, ]
test <- normed1[-train_ind, ]

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
    }, normed1$coverType))
    
    for (h in 2:16)
    {
      cat("Run", i, "for c", c, "and h", h, "\n")
      
      # With normalised data (no need for range now)
      ann <- nnet(train$coverType ~ ., data=normed1[train_ind,-13], weights=data.weights[train_ind], size=h, decay=5e-4, maxit=200)
      pred <- predict(ann, normed1[,-13], type="class")
      tacc <- weighted.acc(pred[train_ind], normed1[train_ind,13])
      wacc <- weighted.acc(pred[-train_ind], normed1[-train_ind,13])
      pacc <- sum(pred[-train_ind]==normed1[-train_ind,2])/length(pred[-train_ind])
      
      results <- rbind(results, c(h, tacc, wacc, pacc, c, 2))
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

plot(results[,3] ~ results[,1], ylim=c(0,1), col=cols, pch=pts, xlab="Hidden neurons", ylab="Weighted accuracy")

best.row <- match(max(results[,3]), results[,3]) #Row 125
best.ann <- models[[best.row]]


#load("U:/R Training//best.ann.RData")

pred1 <- predict(best.ann, normed1[,-13], type="class")

#best model
#Train Accuracy
weighted.acc(pred1[train_ind],normed1[train_ind,13]) #0.8000401   
table(pred1[train_ind],normed1[train_ind,13])

#Test Accuracy
weighted.acc(pred1[-train_ind],normed1[-train_ind,13]) #0.7665194  
table(pred1[-train_ind],normed1[-train_ind,13])


# Save everything 
save(results, file=paste0("U:/Midterm//ann.results2.RData")) 
save(models, file=paste0("U:/Midterm//ann.models2.RData")) 
save(best.ann, file=paste0("U:/Midterm//best.ann2.RData")) 
write.table(results, file=paste0("U:/Midterm//ann.results2.csv"), sep=",") 
