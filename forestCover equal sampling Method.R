
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
                                   to = c("SpruceFir","LodgepolePine","PonderosaPine",
                                          "CottonwoodWillow","Aspen","Douglasfir","Krummholz"))

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
table(data.train$Cover_Type)

names(data.train)


library(neuralnet)
tm_start<-Sys.time()
set.seed(1231239)

### CONVERT THE FACTOR TO DUMMIES ### 
#data.train <- cbind(data.train[, 1:54], class.ind(as.factor(data.train$Cover_Type)))
# Set labels name
#names(wine2) <- c(names(wine)[2:14],"TypeA","TypeB","TypeC")

data.train$C1 <- ifelse(data.train$Cover_Type=='Spruce/Fir',1,0)
data.train$C2 <- ifelse(data.train$Cover_Type=='Lodgepole Pine',1,0)
data.train$C3 <- ifelse(data.train$Cover_Type=='Ponderosa Pine',1,0)
data.train$C4 <- ifelse(data.train$Cover_Type=='Cottonwood/Willow',1,0)
data.train$C5 <- ifelse(data.train$Cover_Type=='Aspen',1,0)
data.train$C6 <- ifelse(data.train$Cover_Type=='Douglas-fir',1,0)
data.train$C7 <- ifelse(data.train$Cover_Type=='Krummholz',1,0)

data.train$Wilderness_Area1 <- as.numeric(data.train$Wilderness_Area1)
data.train$Wilderness_Area2 <- as.numeric(data.train$Wilderness_Area2)
data.train$Wilderness_Area3 <- as.numeric(data.train$Wilderness_Area3)
data.train$Wilderness_Area4 <- as.numeric(data.train$Wilderness_Area4)
data.train$Soil_Type1 <- as.numeric(data.train$Soil_Type1)
data.train$Soil_Type2 <- as.numeric(data.train$Soil_Type2)
data.train$Soil_Type3 <- as.numeric(data.train$Soil_Type3)
data.train$Soil_Type4 <- as.numeric(data.train$Soil_Type4)
data.train$Soil_Type5 <- as.numeric(data.train$Soil_Type5)
data.train$Soil_Type6 <- as.numeric(data.train$Soil_Type6)
data.train$Soil_Type7 <- as.numeric(data.train$Soil_Type7)
data.train$Soil_Type8 <- as.numeric(data.train$Soil_Type8)
data.train$Soil_Type9 <- as.numeric(data.train$Soil_Type9)
data.train$Soil_Type10 <- as.numeric(data.train$Soil_Type10)
data.train$Soil_Type11 <- as.numeric(data.train$Soil_Type11)
data.train$Soil_Type12 <- as.numeric(data.train$Soil_Type12)
data.train$Soil_Type13<- as.numeric(data.train$Soil_Type13)
data.train$Soil_Type14 <- as.numeric(data.train$Soil_Type14)
data.train$Soil_Type15 <- as.numeric(data.train$Soil_Type15)
data.train$Soil_Type16 <- as.numeric(data.train$Soil_Type16)
data.train$Soil_Type17 <- as.numeric(data.train$Soil_Type17)
data.train$Soil_Type18 <- as.numeric(data.train$Soil_Type18)
data.train$Soil_Type19 <- as.numeric(data.train$Soil_Type19)
data.train$Soil_Type20 <- as.numeric(data.train$Soil_Type20)
data.train$Soil_Type21 <- as.numeric(data.train$Soil_Type21)
data.train$Soil_Type22 <- as.numeric(data.train$Soil_Type22)
data.train$Soil_Type23 <- as.numeric(data.train$Soil_Type23)
data.train$Soil_Type24 <- as.numeric(data.train$Soil_Type24)
data.train$Soil_Type25 <- as.numeric(data.train$Soil_Type25)
data.train$Soil_Type26 <- as.numeric(data.train$Soil_Type26)
data.train$Soil_Type27 <- as.numeric(data.train$Soil_Type27)
data.train$Soil_Type28 <- as.numeric(data.train$Soil_Type28)
data.train$Soil_Type29 <- as.numeric(data.train$Soil_Type29)
data.train$Soil_Type30 <- as.numeric(data.train$Soil_Type30)
data.train$Soil_Type31 <- as.numeric(data.train$Soil_Type31)
data.train$Soil_Type32 <- as.numeric(data.train$Soil_Type32)
data.train$Soil_Type33 <- as.numeric(data.train$Soil_Type33)
data.train$Soil_Type34 <- as.numeric(data.train$Soil_Type34)
data.train$Soil_Type35 <- as.numeric(data.train$Soil_Type35)
data.train$Soil_Type36 <- as.numeric(data.train$Soil_Type36)
data.train$Soil_Type37 <- as.numeric(data.train$Soil_Type37)
data.train$Soil_Type38 <- as.numeric(data.train$Soil_Type38)
data.train$Soil_Type39 <- as.numeric(data.train$Soil_Type39)
data.train$Soil_Type40 <- as.numeric(data.train$Soil_Type40)

# data.train$SpruceFir <- as.numeric(data.train$SpruceFir)
# data.train$LodgepolePine <- as.numeric(data.train$LodgepolePine)
# data.train$PonderosaPine <- as.numeric(data.train$PonderosaPine)
# data.train$CottonwoodWillow <- as.numeric(data.train$CottonwoodWillow)
# data.train$Aspen <- as.numeric(data.train$Aspen)
# data.train$Douglasfir <- as.numeric(data.train$Douglasfir)
# data.train$Krummholz <- as.numeric(data.train$Krummholz)

data.train$Cover_Type <- NULL
data.train$train <- NULL

names(data.train)

table(data.train$CottonwoodWillow)

#Normalize the data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
data.train[, 1:54] <- data.frame(lapply(data.train[, 1:54], scl))


names(data.train)
#Quick view of the data
fix(wine2)

#nn model
nn<-neuralnet(data.train$SpruceFir +
              data.train$LodgepolePine +
              data.train$PonderosaPine +
              data.train$CottonwoodWillow +
              data.train$Aspen +
              data.train$Douglasfir +
              data.train$Krummholz +
              ~
                data.train$Elevation +  
                data.train$Aspect +  
                #                 data.train$Slope +  
                data.train$Horizontal_Distance_To_Hydrology +  
                data.train$Vertical_Distance_To_Hydrology +  
                data.train$Horizontal_Distance_To_Roadways +  
                data.train$Hillshade_9am +  
                data.train$Hillshade_Noon +  
                data.train$Hillshade_3pm +  
                data.train$Horizontal_Distance_To_Fire_Points  
                #                 data.train$Wilderness_Area1 +  
                #                  data.train$Wilderness_Area2 +  
                #                  data.train$Wilderness_Area3 +  
                #                  data.train$Wilderness_Area4 +  
              , data=data.train,
              hidden = c(13, 10, 3),
              act.fct = "logistic",
              linear.output = FALSE,
              lifesign = "minimal")
)

Sys.time()







