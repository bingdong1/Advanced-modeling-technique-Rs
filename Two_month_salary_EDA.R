.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")

####################
##Libraries required
####################

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
library(randomForest)
library(rattle)
library(rpart.plot)
library(Metrics)


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

#Load the data
twomonth<-data.table::fread(file="U:/Midterm/two_months_salary.csv",header=TRUE,na.strings=c(""))
twomonth$color = as.factor(twomonth$color)
twomonth$clarity = as.factor(twomonth$clarity)
twomonth$cut = as.factor(twomonth$cut)
twomonth$channel = as.factor(twomonth$channel)
twomonth$store = as.factor(twomonth$store)

#Data Exploration

fix(twomonth)

fix(twomonth)      #quick view of data
dim(twomonth)      # dimensions of data
names(twomonth)    # variable names
str(twomonth)      # one form of summary of data
summary(twomonth$price)  # another form of summary  



boxplot(price~store,data=twomonth, main="Diamond Data", 
        xlab="store", ylab="Price") 
                                              
#find column with NA
which(sapply(twomonth,anyNA))

colSums(sapply(twomonth, is.na))

#########################################  
#Create summaryTable to gather statistics.
#########################################    

summaryTable<-data.table(name=names(twomonth))

#Create summary function for missing data and unique values
summaryTable<-summaryTable[,dataType:= sapply(twomonth,class)]  
summaryTable<-summaryTable[,missing := t(twomonth[,lapply(.SD,function(x) length(which(is.na(x))))]),]
summaryTable<-summaryTable[,unique :=  t(twomonth[,lapply(.SD,function(x) length(unique(x)))]),]

#Create Integer and categorical columns for summary table

integerCols<-summaryTable[dataType %in% c("integer", "numeric"),name]
categoricalCols<-summaryTable[!dataType %in% c("integer", "numeric"),name]

#Create temp dataset with interger columns
temptwomonth<-twomonth[,integerCols,with=FALSE]

#Create summary table for temp dataset
intSummary<-data.table(name=names(temptwomonth))

#Lable measurement for each predictor variable
measuredIn<-c("Elevation","Aspect")
intSummary<-intSummary[,quantity := measuredIn]

#Create summary function for min, max, mean and sd
intSummary<-intSummary[,min :=  t(temptwomonth[,lapply(.SD,function(x) min(x))]),]
intSummary<-intSummary[,max :=  t(temptwomonth[,lapply(.SD,function(x) max(x))]),]
intSummary<-intSummary[,mean :=  t(temptwomonth[,lapply(.SD,function(x) round(mean(x),2))]),]
intSummary<-intSummary[,median :=  t(temptwomonth[,lapply(.SD,function(x) round(median(x),2))]),]
intSummary<-intSummary[,std :=  t(temptwomonth[,lapply(.SD,function(x) round(sd(x),2))]),]

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

featuresToPlot<-c("carat","price")
p=list()

for(i in 1:length(featuresToPlot)){
  p[[i]] <- ggplot(twomonth, aes_string(x=featuresToPlot[i])) + 
    geom_density() + 
    theme_adMobile() + 
    theme(axis.title.y=element_blank())
}
fig_nums("denseNumeric","Density Plot of Numeric Data")

#Call density plot
do.call(grid.arrange,c(p,top=eval(fig_nums("denseNumeric"))))

#### Density Plots & Bar Plots

# Elevation has a relative normal distribution, although it skewed to the left

# price have distribution that's skewed to the right

#Create box plot

fix(twomonth)

p=list()
for(i in 1:length(featuresToPlot)){
  p[[i]] <- ggplot(twomonth, aes_string(y=featuresToPlot[i],x="clarity")) + 
    geom_boxplot(outlier.shape = NA) + 
    theme_adMobile() +
    theme(axis.text.x=element_blank(),axis.title.x=element_blank())
  
}
fig_nums("boxNumeric1","Boxplot of Numeric Data")

#call boxplot
do.call(grid.arrange,c(p,top=eval(fig_nums("boxNumeric1"))))


boxplot(price~clarity,data=twomonth, main="Price versus carat", 
        xlab="Clarity Level", ylab="Sales Price") 




# Correlation plot

plot<-ggcorr(twomonth[,c(1,7)], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE, hjust = 00.75, size = 3,layout.exp = 1)
fig_nums("corrTen","Correlation Matrix of 10 Numerical Features")

#call correlation plot
plot+ggtitle(eval(fig_nums("corrTen")))


#set scatter plot numeric variables

corrFeature1<-c("price")
corrFeature2<-c("carat")

# Scatter plot 
p=list()
for(i in 1:length(corrFeature1)){
  p[[i]] <- ggplot(twomonth, aes_string(x=corrFeature1[i],y=corrFeature2[i])) +
    geom_point(alpha=1/10) +
    theme_adMobile()  
}
fig_nums("scatterPlot7","Scatterplot of Correlated Features")

#call scatter plot
do.call(grid.arrange,c(p,ncol=2,top=eval(fig_nums("scatterPlot6"))))

plot(twomonth$price, twomonth$Carat)

#Categorical Data Exploration

#Bar Chart by color and clarity

categoryCount <- twomonth %>%group_by(color) %>% summarise(count=n())
p1 <- ggplot(categoryCount, aes(x=reorder(color, -count), y=count)) + 
  geom_bar(stat="identity") +
  xlab("color") +
  scale_y_continuous(labels=comma) +
  theme_adMobile() + 
  theme(axis.title.y=element_blank())

categoryCount <- twomonth %>%group_by(clarity) %>% summarise(count=n())
p2 <- ggplot(categoryCount, aes(x=reorder(clarity, -count), y=count)) + 
  geom_bar(stat="identity") +
  xlab("clarity") +
  scale_y_continuous(labels=comma) +
  theme_adMobile() +
  theme(axis.text.x=element_text(size=6),axis.title.y=element_blank()) 

categoryCount <- twomonth %>%group_by(cut) %>% summarise(count=n())
p3 <- ggplot(categoryCount, aes(x=reorder(cut, -count), y=count)) + 
  geom_bar(stat="identity") +
  xlab("cut") +
  scale_y_continuous(labels=comma) +
  theme_adMobile() +
  theme(axis.text.x=element_text(size=6),axis.title.y=element_blank()) 

categoryCount <- twomonth %>%group_by(channel) %>% summarise(count=n())
p4 <- ggplot(categoryCount, aes(x=reorder(channel, -count), y=count)) + 
  geom_bar(stat="identity") +
  xlab("channel") +
  scale_y_continuous(labels=comma) +
  theme_adMobile() +
  theme(axis.text.x=element_text(size=6),axis.title.y=element_blank()) 

categoryCount <- twomonth %>%group_by(store) %>% summarise(count=n())
p5 <- ggplot(categoryCount, aes(x=reorder(store, -count), y=count)) + 
  geom_bar(stat="identity") +
  xlab("store") +
  scale_y_continuous(labels=comma) +
  theme_adMobile() +
  theme(axis.text.x=element_text(size=6),axis.title.y=element_blank()) 


fig_nums("category2","Row Counts of Categorioal Features")

#call scatter plot
grid.arrange(p2,ncol=1,top=eval(fig_nums("category4")))
]
table(twomonth$channel)


#Explortory tree model

#part 2 Exploratory Data Analysis:

#tree model

## Part C - Tree-Based Models

fullTree <- rpart(price ~ ., data = twomonth, control = rpart.control(cp = 0.01))

#summary(fullTree)
# plot(fullTree)
# text(fullTree)

prp(fullTree)					# Will plot the tree
prp(fullTree,varlen=3)				# Shorten variable names

#Create fancy plot
fancyRpartPlot(fullTree, sub = "")

#Spilt data

# Hold-out Test Set
set.seed(8)
train <- sample(nrow(twomonth), nrow(twomonth) * 0.70)
twomonth_train <- twomonth[train,]
twomonth_test <- twomonth[-train,]

# number of observations in Test
nrow(twomonth_test)

# number of observations in Train
nrow(twomonth_train)

par(mfrow = c(2,1))
# Test Distribution
hist(twomonth_test$price, breaks = 20, main = "Test price Distribution")

# Train Distribution
hist(twomonth_train$price, breaks = 20, main = "Train price Distribution")
par(mfrow = c(1,1))

#Naive model backward selection

library(leaps)

mod.bwd = regsubsets(price~., data = twomonth, nvmax = 29, 
                     method = "backward")

bwd.summary = summary(mod.bwd)

names(bwd.summary)

which.max(bwd.summary$adjr2) 

#Adjusted R2 plot
plot(bwd.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, 
     type = "l")
points(25, bwd.summary$adjr2[25], pch = 4, col = "red", lwd = 7)

coefi <- coef(mod.bwd, id = 25)

#Goodness of fit

modelNaive = lm(price ~ .,data=twomonth_train)
summary(modelNaive)

names(modelNaive)

par(mfrow=c(2,2)) 
plot(modelNaive)
par(mfrow=c(1,1))

#Variable Selection method

length(twomonth_train)
regfit.fwd <- regsubsets(price~., data = twomonth_train,
                         nvmax = 29, method = "exhaustive")

summary(regfit.fwd)

#validation error for test
val.errors <- rep(NA, 29)
# make test model matrix, with Salary being the response
x.test <- model.matrix(price~., data = twomonth_test)
# now make predictions for each model
for(i in 1:29) {
  coefi = coef(regfit.fwd, id = i)
  pred = x.test[ , names(coefi)] %*% coefi
  val.errors[i] = mean( (twomonth_test$price-pred)^2 )
} 

#validation for training
val.errors2 <- rep(NA, 29)
# make test model matrix, with Salary being the response
x.test2 <- model.matrix(price~., data = twomonth_train)
# now make predictions for each model
for(i in 1:29) {
  coefi = coef(regfit.fwd, id = i)
  pred = x.test2[ , names(coefi)] %*% coefi
  val.errors2[i] = mean( (twomonth_train$price-pred)^2 )
} 


plot(val.errors, ylab="MSE",
     ylim=c(0, 5000000),pch=19, type="b")
points(val.errors2, col="blue", pch=19, type="b")
legend("topright", legend=c("Training", "Validation"),
       col=c("blue", "black"), pch=19)

# I want the 29 variable model
coefi <- coef(regfit.fwd, id = 29)


#Variable selection

leaps <- regsubsets(price ~.,
                 data=twomonth, nbest=10)

plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

# lasso
library(glmnet)
twomonth_train_y <- twomonth$price
twomonth_train_x <- model.matrix(price ~., data = twomonth)[, -7]


# charity_test_y <- charity_test$DAMT 
# charity_test_x <- model.matrix(DAMT ~., data = charity_test)[, -1]

# Lasso model
lasso.cv <- cv.glmnet(twomonth_train_x, twomonth_train_y, alpha = 1)
plot(lasso.cv)
best_lambda_lasso <- lasso.cv$lambda.min

plot(lasso.cv$glmnet.fit, xvar="lambda", label=TRUE)
lasso.cv$lambda.min
lasso.cv$lambda.1se
coef(lasso.cv, s=lasso.cv$lambda.min)

#model construction

############################################
#LM with no interaction terms
############################################

library(MASS)
modelLM = lm(price ~ ., data=twomonth_train)
summary(mystep1)

anova(modelLM)

mystep1=stepAIC(modelLM, direction="both")

mypredLMTrain=predict(mystep1,twomonth_train)

par(mfrow=c(2,2)) 
plot(mystep1)
par(mfrow=c(1,1))

#Calculate RMSE for training set

lm_train <- rmse(twomonth_train$price,mypredLMTrain) #1237

#Calculate RMSE for test set

mypredLMTest=predict(mystep1,twomonth_test)

lm_test <- rmse(twomonth_test$price,mypredLMTest) #1470

############################################
#LM with interaction terms
############################################

twomonth_train$ColorClarity <- with(twomonth_train, interaction(color,  clarity))

twomonth_test$ColorClarity <- with(twomonth_test, interaction(color,  clarity))


modelLM2 = lm(price ~ ColorClarity + cut + channel + store + carat, data=twomonth_train)
summary(modelLM2)

par(mfrow=c(2,2)) 
plot(modelLM2)
par(mfrow=c(1,1))


mypredLM2Train=predict(modelLM2,twomonth_train)

#Calculate RMSE for training set

lm2_train <- rmse(twomonth_train$price,mypredLM2Train) #1060

#Calculate RMSE for test set

mypredLM2Test=predict(modelLM2,twomonth_test)

#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#factor ColorClarity has new levels 3.9

#Drop the new factors
twomonth_test2 <- twomonth_test

twomonth_test2<-twomonth_test2[!(twomonth_test2$ColorClarity=="3.9"),]

mypredLM2Test=predict(modelLM2,twomonth_test2)

lm2_test <- rmse(twomonth_test2$price,mypredLM2Test) #1668


############################################
## Tree-Based Models
############################################

fullTree <- rpart(price ~ color + clarity + cut + store + carat, data = twomonth_train)



prp(fullTree)					# Will plot the tree
prp(fullTree,varlen=5)				# Shorten variable names

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
prp(pruneTree,varlen=3)

#Create fancy plot
fancyRpartPlot(pruneTree, sub = "")

mypredTreeTrain = predict(pruneTree,newdata=twomonth_train, type="vector")

#Calculate RMSE for training set

tree_train <- rmse(twomonth_train$price,mypredTreeTrain) #1558

#Calculate RMSE for test set

mypredTreeTest=predict(pruneTree,twomonth_test)

tree_test <-rmse(twomonth_test$price,mypredTreeTest) #1666


############################################
#Random Forest
############################################

library(randomForest)
set.seed(711)
modelRF <- randomForest(price ~color + clarity + cut + store + carat,
                        data = twomonth_train, 
                        importance = TRUE, 
                        ntree = 500,
                        mtry = 3)

varImpPlot(modelRF)

summary(modelRF)

mypredRFTrain=predict(modelRF, newdata=twomonth_train)

#Calculate RMSE for training set

rf_train <- rmse(twomonth_train$price,mypredRFTrain) #741

#Calculate RMSE for test set

mypredRFTest=predict(modelRF,twomonth_test)

rf_test <-rmse(twomonth_test$price,mypredRFTest) #1446


# Build a table
# three columns - Model Name, Training Set MSE, Test Set MSE
Model_names <- c('LM with no interatcion', 'LM with interatcion', 'Tree', 'Random Forest')
training_MSE <- c(lm_train, lm2_train, tree_train, rf_train)
Test_MSE <- c(lm_test, lm2_test, tree_test, rf_test)


results_table <- as.data.frame(Model_names)
results_table$training_MSE <- training_MSE
results_table$Test_MSE <- Test_MSE
results_table








