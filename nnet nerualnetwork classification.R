
#rm(list=ls()) 
.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")


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
library(dummies)
library(nnet)

#load data from url

UCI <- "https://archive.ics.uci.edu/ml/"
REPOS <- "machine-learning-databases"
wine.url <- sprintf("%s/%s/wine/wine.data", UCI, REPOS)
wine <- read.csv(wine.url, header=F) 
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
wine$Type <- as.factor(wine$Type)
write.table(wine, "wine.csv", sep=",", row.names=FALSE)
save(wine, file="wine.Rdata", compress=TRUE)

load("wine.Rdata")

?wine

#1. Data Quality Check:

fix(wine)      #quick view of data
dim(wine)      # dimensions of data
names(wine)    # variable names
str(wine)      # one form of summary of data
summary(wine)  # another form of summary

table(wine$Type)

#find column with NA
which(sapply(wine,anyNA))

colSums(sapply(wine, is.na))


#Create accuracy function
weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

#Random Forest

library(randomForest)
set.seed(711)
modelRF <- randomForest(Type ~. ,
                        data = wine, 
                        importance = TRUE, 
                        ntree = 500,
                        mtry = 3)

#Importance Plot
varImpPlot(modelRF)

importance(modelRF)

#RF model summary
summary(modelRF)

#Prediction Training
mypredRF=predict(modelRF, data=wine)

weighted.acc(mypredRF,wine$Type) #0.9802657754

confusionMatrix(mypredRF,wine$Type)

#Or there will be a "not a valid R variable name" error 
wine$Type[wine$Type == 1] = "wineA"
wine$Type[wine$Type == 2] = "wineB"
wine$Type[wine$Type == 3] = "wineC"




#make column y a factor variable for binary classification (spam or non-spam)
wine$Type <- as.factor(wine$Type)


### finding optimal value of a tuning parameter
sigDist <- sigest(Type ~ ., data = wine, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

x <- train(Type ~ .,
           data = wine,
           method = "svmRadial",
           preProc = c("center", "scale"),
           tuneGrid = svmTuneGrid,
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  TRUE))

#Predict train data
predSVM <- predict(x,wine[,2:14])

weighted.acc(predSVM,wine$Type) #0.9953051643

confusionMatrix(predSVM,wine$Type)

library(kernlab)


table(wine$Type)

library(neuralnet)
tm_start<-Sys.time()
set.seed(1231239)

### CONVERT THE FACTOR TO DUMMIES ### 
library(neuralnet)
neuralnet

wine2 <- cbind(wine[, 2:14], class.ind(as.factor(wine$Type)))
# Set labels name
names(wine2) <- c(names(wine)[2:14],"TypeA","TypeB","TypeC")

#Normalize the data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
wine2[, 1:13] <- data.frame(lapply(wine2[, 1:13], scl))

#Quick view of the data
fix(wine2)

#nn model
nn<-neuralnet(wine2$TypeA + wine2$TypeB + wine2$TypeC
              ~ wine2$Alcohol + wine2$Malic + wine2$Ash + wine2$Alcalinity + wine2$Magnesium + wine2$Phenols + wine2$Flavanoids + wine2$Nonflavanoids + wine2$Proanthocyanins + wine2$Color + wine2$Hue + wine2$Dilution + wine2$Proline
              , data=wine2,
              hidden = c(13, 10, 3),
              act.fct = "logistic",
              linear.output = FALSE,
              lifesign = "minimal")
tm_start
Sys.time()

#Print the result
print(nn)

#Plot the neural network
plot(nn)

#Test the neural network 
nn.results<-compute(nn,covariate=wine2[,1:13]) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(nn.results)

#Lets see the results
print(nn.results$net.result)

pr.nn_ <- nn.results$net.result

# Accuracy (training set)
original_values <- max.col(wine2[, 14:16])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

#Predict accuracy
confusionMatrix(pr.nn_2,wine$Type) #1


#Weighted ANN Model

#Take sample of the original dataset since it take too long to run this model, over 8 hours for the full dataset
# sampleIndex <- createDataPartition(normed$coverType, p = .1,list = FALSE,times = 1)
# normed1 <- normed[ sampleIndex,]
# coverData1 <- coverData1[ sampleIndex,]

# require('caret')
# require('randomForest')

## 70% of the sample size

smp_size <- floor(0.70 * nrow(wine))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(wine)), size = smp_size)

train <- wine[train_ind, ]
test <- wine[-train_ind, ]

#Normalized the data
normed <- cbind(as.data.frame(lapply(wine[,-c(1)], function(col) { col / max(abs(col)) })),wine$Type)

normed$Type <- normed$`wine$Type`

normed$Type <- as.factor(normed$Type)

normed$`wine$Type` <- NULL

#Set the weight class
cw1 <- rep(1, 3) # all equal
cw2 <- c(1,10,100) # powers of 10
freqs <- as.data.frame(table(normed$Type))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(wine[,14])/as.integer(s[2])})) # 1/counts

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("TypeA","TypeB","TypeC")

#Check the train and test to make sure it has all covertype
#table(test$coverType)
#table(test$coverType)

results <- matrix(ncol=6, nrow=0) 
models <- list()

# Do not run for 3 interations, 1 iteration takes over 10 hours 
# Consider taking another sample

for (i in 1:3)
{
  for (c in 1:length(class.weights[,1]))
  {
    data.weights <- do.call(rbind, Map(function(s)
    {
      class.weights[c,s]
    }, normed$Type))
    
    for (h in 1:13)
    {
      cat("Run", i, "for c", c, "and h", h, "\n")
      
      # With normalised data (no need for range now)
      ann <- nnet(train$Type ~ ., data=normed[train_ind,-14], weights=data.weights[train_ind], size=h, decay=5e-4, maxit=200)
      pred <- predict(ann, normed[,-14], type="class")
      tacc <- weighted.acc(pred, normed[,14])
      wacc <- weighted.acc(pred[-train_ind], normed[-train_ind,14])
      pacc <- sum(pred[-train_ind]==normed[-train_ind,2])/length(pred[-train_ind])
      
      results <- rbind(results, c(h, tacc, wacc, pacc, c, 2))
      models[[(length(models)+1)]] <- ann
      
      
      #With neither range nor normalisation
            ann <- nnet(train$Type ~ ., data=wine[train_ind,-1], weights=data.weights[train_ind], size=h, decay=5e-4, maxit=200)
            pred <- predict(ann, wine[,-1], type="class")
            tacc <- weighted.acc(pred, wine[,1])
            wacc <- weighted.acc(pred[-train_ind], wine[-train_ind,1])
            pacc <- sum(pred[-train_ind]==wine[-train_ind,2])/length(pred[-train_ind])
            
            results <- rbind(results, c(h, tacc, wacc, pacc, c, 3))
            models[[(length(models)+1)]] <- ann
      
      
      
    }
  }
}


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


best.row <- match(max(results[,3]), results[,3]) #Row 17
best.ann <- models[[best.row]]


prednnet <- predict(best.ann, wine[,-1], type="class")

#best model
#Train Accuracy
weighted.acc(prednnet,wine[,1]) #0.9840057   




