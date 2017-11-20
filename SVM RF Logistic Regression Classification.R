rm(list=ls()) 
.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")

#Data work 
require(data.table) #Working with large files
#require(xlsx)       #Loading and saving .xlsx files 
require(plyr)   #Always load in this order plyr, dpply, lubridate - dpplyr overrides some of methods in plyr. 
require(dplyr) #Use require as it will give an error message if the package doesn't exist
require(lubridate) #used for working with data information. 
require(reshape2)  #used for melting 
require(ggplot2)
require(ggthemes)
require(scales)
require(GGally)
#require(cowplot)
require(gridExtra)
library(rpart)
library(rattle)
library(rpart.plot)
library(Metrics)


boxplot(dataset$word_freq_you, dataset$y)

#Formating and printing 
#install.packages("devtools")
#devtools::install_github("adletaw/captioner")   #Nice library for numbering and captioning tables in conjunction with knitr and pandoc
require(pander)   	#for creating nice output tables.
require(captioner)

#Set up the figure and table numbering
fig_nums<-captioner(prefix = "Figure")
tab_nums<-captioner(prefix = "Table")

#Using pryr abbreviate how to call fig_nums function 
require(pryr)
citefig<-pryr::partial(fig_nums,display="cite")
citetab<-pryr::partial(tab_nums,display="cite")

#Turn off caption.prefix as allow captioner to handle this. 
panderOptions('table.caption.prefix', '')
panderOptions('big.mark', ",")
panderOptions('keep.trailing.zeros', TRUE)
panderOptions('keep.line.breaks',TRUE)
panderOptions('table.emphasize.rownames',TRUE)

#Create theme for ggplots
theme_adMobile <- function(){
  theme(
    axis.text.x=element_text(angle=30,hjust=1,size=8),
    axis.text.y=element_text(size=8),
    axis.title.x=element_text(size=10),
    axis.title.y=element_text(size=10),
    panel.background = element_blank(),
    title=element_text(size=16))
}

#Download Data Files: 
#data.csv:  http://thinktostart.com/data/data.csv 
#names.csv:  http://thinktostart.com/data/names.csv

#Load the two files into R: 
dataset<-data.table::fread(file="U:/Midterm/data.csv",header=TRUE,na.strings=c(""))
#dataset <- read.csv(file="U:/Midterm/data.csv",header=FALSE,sep=";") 
names <- read.csv(file="U:/Midterm/names1.csv",header=FALSE,sep=";") 

#Set the names of the dataset dataframe:
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))

#Or there will be a "not a valid R variable name" error 
dataset$y[dataset$y == 1] = "yes"
dataset$y[dataset$y == 0] = "no"

#make column y a factor variable for binary classification (spam or non-spam)
dataset$y <- as.factor(dataset$y)

#Create accuracy function
weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

#Data Exploration

fix(dataset)      #quick view of data
dim(dataset)      # dimensions of data
names(dataset)    # variable names
str(dataset)      # one form of summary of data
summary(dataset)  # another form of summary  

#find column with NA
which(sapply(dataset,anyNA))

colSums(sapply(dataset, is.na))

#########################################  
#Create summaryTable to gather statistics.
#########################################    

summaryTable<-data.table(name=names(dataset))

#Create summary function for missing data and unique values
summaryTable<-summaryTable[,dataType:= sapply(dataset,class)]  
summaryTable<-summaryTable[,missing := t(dataset[,lapply(.SD,function(x) length(which(is.na(x))))]),]
summaryTable<-summaryTable[,unique :=  t(dataset[,lapply(.SD,function(x) length(unique(x)))]),]

#Create Integer and categorical columns for summary table

integerCols<-summaryTable[dataType %in% c("integer", "numeric"),name]
categoricalCols<-summaryTable[!dataType %in% c("integer", "numeric"),name]

#Create temp dataset with interger columns
tempdataset<-dataset[,integerCols,with=FALSE]

#Create summary table for temp dataset
intSummary<-data.table(name=names(tempdataset))

#Lable measurement for each predictor variable
measuredIn<-c("Elevation","Aspect")
intSummary<-intSummary[,quantity := measuredIn]

#Create summary function for min, max, mean and sd
intSummary<-intSummary[,min :=  t(tempdataset[,lapply(.SD,function(x) min(x))]),]
intSummary<-intSummary[,max :=  t(tempdataset[,lapply(.SD,function(x) max(x))]),]
intSummary<-intSummary[,mean :=  t(tempdataset[,lapply(.SD,function(x) round(mean(x),2))]),]
intSummary<-intSummary[,median :=  t(tempdataset[,lapply(.SD,function(x) round(median(x),2))]),]
intSummary<-intSummary[,std :=  t(tempdataset[,lapply(.SD,function(x) round(sd(x),2))]),]

#merge summary table
summaryTable<-merge(summaryTable,intSummary,by="name",all=TRUE,sort=FALSE)

#Create final temp Table  
mytable<-data.frame(summaryTable)
tab_nums("sumTab","Summary of CoverTye Dataset Featuers")

#Create Summary Table
pandoc.table(mytable,caption=tab_nums("sumTab"),justify=c("left"),missing="",split.table=Inf)


############################   
# Numerical Data Exploration  
############################  

#Create denstiy plot

names(dataset)

featuresToPlot<-c("word_freq_mail","word_freq_people","word_freq_free","word_freq_you","word_freq_font","word_freq_hp","word_freq_650","word_freq_make","word_freq_address","word_freq_all","word_freq_people", "word_freq_money" )
p=list()

for(i in 1:length(featuresToPlot)){
  p[[i]] <- ggplot(dataset, aes_string(x=featuresToPlot[i])) + 
    geom_density() + 
    theme_adMobile() + 
    theme(axis.title.y=element_blank())
}
fig_nums("denseNumeric","Density Plot of Numeric Data")

#Call density plot
do.call(grid.arrange,c(p,top=eval(fig_nums("denseNumeric"))))


#Create box plot


p=list()
for(i in 1:length(featuresToPlot)){
  p[[i]] <- ggplot(dataset, aes_string(y=featuresToPlot[i],x="y")) + 
    geom_boxplot(outlier.shape = NA) + 
    theme_adMobile() +
    theme(axis.text.x=element_blank(),axis.title.x=element_blank())
  
}
fig_nums("boxNumeric1","Boxplot of Numeric Data")

#call boxplot
do.call(grid.arrange,c(p,top=eval(fig_nums("boxNumeric1"))))


# Correlation plot

plot<-ggcorr(dataset[,1:57,with=FALSE], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE, hjust = 00.75, size = 3,layout.exp = 1)
fig_nums("corrTen","Correlation Matrix of 57 Numerical Features")

#call correlation plot
plot+ggtitle(eval(fig_nums("corrTen")))

corr <- cor(dataset[,1:57])


#write.table(corr, file=paste0("U:/Midterm//corr.csv"), sep=",") 

#Explortory tree model

#part 2 Exploratory Data Analysis:

#tree model

## Part C - Tree-Based Models

fullTree <- rpart(y ~ ., data = dataset, control = rpart.control(cp = 0.01))

#summary(fullTree)
# plot(fullTree)
# text(fullTree)

prp(fullTree)					# Will plot the tree
prp(fullTree,varlen=3)				# Shorten variable names

#Create fancy plot
fancyRpartPlot(fullTree, sub = "")


require(caret)
require(kernlab)

#Split the data in dataTrain and dataTest

trainIndex <- createDataPartition(dataset$y, p = .7, list = FALSE, times = 1)
dataTrain <- dataset[ trainIndex,]
dataTest  <- dataset[-trainIndex,]

#Logistic Regression

#Boruta variable selection

#Boruta
library(Boruta)

train <- coverData1[sampleIndex,]

train_Boruta <- dataTrain[,1:57] 

train <- coverData1[sampleIndex,]

bor.results <- Boruta(train_Boruta, dataTrain$y,
                      maxRuns=100)

plot(bor.results)
names(bor.results)
bor.results$finalDecision
CONFIRMED_VAR <- getSelectedAttributes(bor.results)
Boruta_output <- attStats(bor.results)

#confirmed all variables except for 2


#Logistic Regression

table(dataTrain$y)

glm.fits=glm(y~.,dataTrain,family=binomial(link = "logit"))

glm.probs=predict(glm.fits,type='response')

hist(glm.probs,col="gray")   # Note that scores are distributed around 0.05.
hist(glm.probs,col="gray",xlim=c(0,1))  

library(pROC)
library(lift)


# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA = roc(response=dataTrain$y,predictor=glm.probs)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA$auc,digits=3),sep=""))
#0.617
par(pty="m")

dist= sqrt((rocA$specificities-1)^2 + (rocA$sensitivities-1)^2)
optIdxA = which.min(dist)  # index corresponding to minimum distance
#generate the threshold
threshA = rocA$thresholds[optIdxA]  # threshold corresponding to min. distance
#0.4031289

mypredLM <- ifelse(glm.probs>threshA, "yes", "no") 


#Train data
weighted.acc(mypredLM,dataTrain$y) #0.9315156


#Test data
glm.probs2=predict(glm.fits,newdata = dataTest, type='response')


# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA2 = roc(response=dataTest$y,predictor=glm.probs2)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA2,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA2$auc,digits=3),sep=""))
#0.617
par(pty="m")

dist2= sqrt((rocA2$specificities-1)^2 + (rocA2$sensitivities-1)^2)
optIdxA2 = which.min(dist2)  # index corresponding to minimum distance
#generate the threshold
threshA2 = rocA2$thresholds[optIdxA2]  # threshold corresponding to min. distance
#0.4260568

mypredLM2 <- ifelse(glm.probs2>threshA2, "yes", "no") 

#Test data
weighted.acc(mypredLM2,dataTest$y) #0.9327126


############################################
## Tree-Based Models
############################################

fullTree <- rpart(y ~., data = dataTrain)


prp(fullTree)					# Will plot the tree

summary(fullTree)

#Create fancy plot
fancyRpartPlot(fullTree, sub = "")

# Prune the tree
printcp(fullTree)
plotcp(fullTree)

cpBest = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]),"CP"]
pruneTree = prune(fullTree,cp=cpBest) # In this case, the optimal tree is the unpruned tree

#summary(modelC1)
prp(pruneTree)

#Create fancy plot
fancyRpartPlot(pruneTree, sub = "")

mypredTreeTrain = predict(pruneTree,newdata=dataTrain, type="class")

#Train data
weighted.acc(mypredTreeTrain,dataTrain$y) #0.8858902

mypredTreeTest = predict(pruneTree,newdata=dataTest, type="class")

#Test data
weighted.acc(mypredTreeTest,dataTest$y) #0.8973429


#Random Forest

library(randomForest)
set.seed(711)
modelRF <- randomForest(y ~. ,
                        data = dataset, 
                        importance = TRUE, 
                        ntree = 500,
                        mtry = 3)

#Importance Plot
varImpPlot(modelRF)

#RF model summary
summary(modelRF)

#Prediction Training
mypredRFTrain=predict(modelRF, newdata=dataTrain)

weighted.acc(mypredRFTrain,dataTrain$y) #0.9827027

#Prediction Test
mypredRFTest=predict(modelRF, newdata=dataTest)

weighted.acc(mypredRFTrain,dataTest$y) #0.9868333

confusionMatrix(mypredRFTrain,dataTest$y)


#SVM

require(caret)
require(kernlab)

#Create the SVM model:

### finding optimal value of a tuning parameter
sigDist <- sigest(y ~ ., data = dataTrain, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

x <- train(y ~ .,
           data = dataTrain,
           method = "svmRadial",
           preProc = c("center", "scale"),
           tuneGrid = svmTuneGrid,
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  TRUE))

#Predict train data
predSVMTrain <- predict(x,dataTrain[,1:57])

weighted.acc(predSVMTrain,dataTrain$y) #0.9543714

#Predict train data
predSVMTest <- predict(x,dataTest[,1:57]) #0.9309095

weighted.acc(predSVMTest,dataTest$y)

#Confusion matrix
confusionMatrix(predSVMTest,dataTest$y)




