
# .libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")
# install.packages('rattle')

library(lattice)
library(caret)
library(leaps)
library(pls)
library(pscl)
library(MASS)
library(boot)
library(pROC)
library(lift)
library(rpart)
library(randomForest)
library(rattle)
library(rpart.plot)
library(cluster)
library(fpc)
library(ISLR)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

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

#Separate numeric variable from categorical ones
cont_cols <- colnames(wine)[sapply(wine, function(x) length(unique(x)))>10]
cat_cols <- setdiff(colnames(wine),cont_cols)

#create summary data for continuous variables
res_cont <- data.frame(do.call(rbind,lapply(wine[,cont_cols],summary)))

#add column with variable name (unwise to store in rownames)
res_cont$variable <- rownames(res_cont)

#create summary data for categorical variables
res_cat <- do.call(rbind,lapply(cat_cols, function(x){
  res <- data.frame(table(wine[,x],useNA="always")) #added it to deal with missings, can be changes
  res$variable <- x
  colnames(res)[1] <- "Type"
  res
}
))

#find column with NA, no missing value within this dataset
which(sapply(wine,anyNA))

colSums(sapply(wine, is.na))

# bar plot for response variable
barplot(table(wine$Type), xlab="Type")

plot(wine$Alcohol,wine$Type)

#list out names for the wine dataset
wine_names <- names(wine)

# we do not want  the response variables for EDA
response_var <- c("Type")
wine_predictor <- wine[ , !(names(wine) %in% response_var)]
# 
# varlist <- names(wine)[2:14]

# for (i in names(wine)){
#   p <- histogram(~ wine[[i]]| factor(Type), data = wine)
#   plot(p)
# }

#EDA function
with(wine, do.call(rbind, tapply(Proline, Type, function(x) c(M = mean(x), SD = sd(x)))))

hist(wine$Proline)

par(mfrow = c(2,2)) # play with breaks and update title
histogram(~ Alcohol | factor(Type), data = wine)
histogram(~ Malic | factor(Type), data = wine)
histogram(~ Ash | factor(Type), data = wine)
histogram(~ Alcalinity | factor(Type), data = wine)

par(mfrow = c(2,2)) # play with breaks and update title
histogram(~ Magnesium | factor(Type), data = wine)
histogram(~ Phenols | factor(Type), data = wine)
histogram(~ Flavanoids | factor(Type), data = wine)
histogram(~ Nonflavanoids | factor(Type), data = wine)

par(mfrow = c(2,2)) # play with breaks and update title
histogram(~ Proanthocyanins | factor(Type), data = wine)
histogram(~ Color | factor(Type), data = wine)
histogram(~ Hue | factor(Type), data = wine)
histogram(~ Dilution | factor(Type), data = wine)

par(mfrow = c(1,1)) 
histogram(~ Proline | factor(Type), data = wine)

#Create function for density plot
indiv_plots_density <- function(x) {
  
  densityplot(~ x, data = wine, groups = Type,
              plot.points = FALSE,
              auto.key = list(space = "right", title = "score"))
}

par(mfrow = c(2,2)) # play with breaks and update title
indiv_plots_density(wine$Alcohol)
indiv_plots_density(wine$Malic)
indiv_plots_density(wine$Ash)
indiv_plots_density(wine$Alcalinity)

par(mfrow = c(2,2)) # play with breaks and update title
indiv_plots_density(wine$Magnesium)
indiv_plots_density(wine$Phenols)
indiv_plots_density(wine$Flavanoids)
indiv_plots_density(wine$Nonflavanoids)

par(mfrow = c(2,2)) # play with breaks and update title
indiv_plots_density(wine$Proanthocyanins)
indiv_plots_density(wine$Color)
indiv_plots_density(wine$Hue)
indiv_plots_density(wine$Dilution)

par(mfrow = c(1,1)) 
indiv_plots_density(wine$Proline)

#set outlier threshold

outlier_threshold = 2.5

#set up outliers column
for(i in names(wine_predictor)){
  wine_predictor[[paste(i, 'outlier', sep="_")]] <- mean(wine_predictor[[i]]) + (outlier_threshold * sd(wine_predictor[[i]]))
  wine_predictor[[paste(i, 'out_Flag', sep="_")]] <- ifelse(wine_predictor[[i]] >= (mean(wine_predictor[[i]]) + (outlier_threshold * sd(wine_predictor[[i]]))), 1, 0)
}
fix(wine_predictor)


#part 2 Exploratory Data Analysis:

#tree model

## Part C - Tree-Based Models
fullTree = rpart(Type ~  .,
                 data=wine,
                 method="class")
#summary(fullTree)
plot(fullTree)
text(fullTree)

#Create fancy plot
fancyRpartPlot(fullTree, sub = "")

# Prune the tree
printcp(fullTree)
cpBest = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]),"CP"]
pruneTree = prune(fullTree,cp=cpBest) # In this case, the optimal tree is the unpruned tree
#summary(modelC1)
plot(pruneTree)
text(pruneTree,pretty=0)

#Create fancy plot
fancyRpartPlot(pruneTree, sub = "")

## Part 3 Model Based Exploratory Data Analysis

#principal component analysis

# PCA only works with quantitative variables, I also removed the response variables and ID variable
# we do not want to skew the PCA results by leaving in the response variables
qualitative_vars <- c("Type")
wine_quant <- wine[ , !(names(wine) %in% qualitative_vars)]

apply(wine_quant, 2, mean)
apply(wine_quant, 2, sd)

pca.out <- prcomp(wine_quant, scale = TRUE)
pca.out

wine_results <- wine
wine_results$PC1 <- pca.out$x[,1]
wine_results$PC2 <- pca.out$x[,2]
plot(wine_results$PC1, wine_results$PC2, col = wine_results$Type, xlab = "PC1", ylab = "PC2", pch = 19)

## Part B - Generate and show a scree plot
pr.var <- pca.out$sdev^2
pve <- pr.var / sum(pr.var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')


## Part BC - Generate and include a bigplot
biplot(pca.out, scale = 0, cex = 0.6) #, display = c("AGE", "MEDAGE"))
# remove a few of the variables to improve legibility 

#clustering K MEANS
km.out =kmeans (wine_quant,3, nstart =20)

km.out$cluster

plotcluster(wine_quant, km.out$cluster)

clusplot(wine_quant, km.out$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

with(wine, pairs(wine_quant, col=c(1:3)[km.out$cluster])) 

#Confusion matrix
confMattrix_a1_train = table(wine$Type, km.out$cluster, dnn=c("Target","Predicted"))

#Hierarchical Clustering

hc.complete =hclust(dist(wine_quant), method ="complete")
hc.average =hclust (dist(wine_quant), method ="average")
hc.single =hclust (dist(wine_quant), method ="single")

par(mfrow =c(1,3))
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="",
       cex =.9)
plot(hc.average , main =" Average Linkage ", xlab="", sub ="",
       cex =.9)
plot(hc.single , main=" Single Linkage ", xlab="", sub ="",
       cex =.9)

cutree (hc.complete , 3)

xsc=scale(wine_quant)
plot(hclust (dist(xsc), method ="complete"), main ="Hierarchical
       Clustering with Scaled Features")

table(cutree(hc.complete , 3))

hc_complete_3 <- cutree(hc.complete , 3)

table(wine$Type)

confMattrix_a1_train = table(wine$Type, hc_complete_3, dnn=c("Target","Predicted"))

#fit simple logistic model, does not work
model <- glm(Type ~.,family=binomial(link='logit'),data=wine)

summary(model)



#Multinomial logistic regression
wine$Type2 <- relevel(wine$Type, ref = 1)
test <- multinom(Type ~., data = wine)
summary(test)

#Create prediction function 
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}

y2 <- predictMNL(test,wine)

y2$fitted.values

df2 <- cbind(wine,y=y2)

table(wine$Type, y2, dnn=c("Target","Predicted"))

set.seed(711)
random_forest <- randomForest(Type ~ .,
                        data = wine, 
                        importance = TRUE, 
                        ntree = 500,
                        mtry = 3)

varImpPlot(random_forest)



