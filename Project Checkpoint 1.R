# title: "Team Checkpoint 1"
# subtitle: "Forest Cover Type Prediction"
# EDA

#.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")

# rm(list=ls()) 
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


#High level data overview

fix(coverData1)      #quick view of data
dim(coverData1)      # dimensions of data
names(coverData1)    # variable names
str(coverData1)      # one form of summary of data
summary(coverData1)  # another form of summary  

#High level Summary

# There are 581012 rows across 55 columns. 
#   
# columns 1-10 are quantiative data. 
#   
# columns 11-14 are a binary representation of Wilderness_Area. (4 columns are mutually exlusive; ComanchePeak, CachePoudre, Neota, Rawah)
#     
# columns 15-54 are a binary representation of Soil_Type. (40 columns are  mutually exclusive)
#     
# column **Cover_Type** has 7 values (target for prediction)
  
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
  
  #check new variable name
  
  names(coverData1)
  
  #remove the binary columns
  coverData1 <- coverData1[ , colnames(coverData1[,11:54,with=FALSE]):=NULL]
  
  #reorder the columns
  colOrder<-c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Horizontal_Distance_To_Fire_Points","Hillshade_9am ","Hillshade_Noon","Hillshade_3pm","wildernessArea","soilType","Cover_Type")  
  
  setcolorder(coverData1, colOrder)
  
  #Shorten names for readability.
  newNames<-c("Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire","HS.9am","HS.noon","HS.3pm","wildernessArea","soilType","coverType")
  setnames(coverData1,colOrder,newNames)
  
#########################################  
#Create summaryTable to gather statistics.
#########################################    
  
  summaryTable<-data.table(name=names(coverData1))
  
  #Create summary function for missing data and unique values
  summaryTable<-summaryTable[,dataType:= sapply(coverData1,class)]  
  summaryTable<-summaryTable[,missing := t(coverData1[,lapply(.SD,function(x) length(which(is.na(x))))]),]
  summaryTable<-summaryTable[,unique :=  t(coverData1[,lapply(.SD,function(x) length(unique(x)))]),]
  
  #Create Integer and categorical columns for summary table
  integerCols<-summaryTable[dataType=="integer",name]
  categoricalCols<-summaryTable[dataType!="integer",name]
  
  #Create temp dataset with interger columns
  tempCoverData<-coverData1[,integerCols,with=FALSE]
  
  #Create summary table for temp dataset
  intSummary<-data.table(name=names(tempCoverData))
  
  #Lable measurement for each predictor variable
  measuredIn<-c("meters","azimuth","degrees","meters","meters","meters","meters","0-255","0-255","0-255")
  intSummary<-intSummary[,quantity := measuredIn]
  
  #Create summary function for min, max, mean and sd
  intSummary<-intSummary[,min :=  t(tempCoverData[,lapply(.SD,function(x) min(x))]),]
  intSummary<-intSummary[,max :=  t(tempCoverData[,lapply(.SD,function(x) max(x))]),]
  intSummary<-intSummary[,mean :=  t(tempCoverData[,lapply(.SD,function(x) round(mean(x),2))]),]
  intSummary<-intSummary[,std :=  t(tempCoverData[,lapply(.SD,function(x) round(sd(x),2))]),]
  
  #merge summary table
  summaryTable<-merge(summaryTable,intSummary,by="name",all=TRUE,sort=FALSE)

  #Create final temp Table  
  mytable<-data.frame(summaryTable)
  tab_nums("sumTab","Summary of CoverTye Dataset Featuers")
  
  #Create Summary Table
  pandoc.table(mytable,caption=tab_nums("sumTab"),justify=c("left"),missing="",split.table=Inf)

#####################  
# Class Distribution
#####################   
  
  #tempTable using dplyr
  coverType.cnt = coverData1%>% 
    group_by(coverType) %>% 
    summarise(count=n()) %>% 
    mutate(pct=count/sum(count)) 
  
  #Create Bar plot for CoverType
  plot<-ggplot(coverType.cnt, aes(x=coverType, y=count)) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels=comma) +
    geom_text(data=coverType.cnt, aes(label=count,y=count+5000),size=4) +
    geom_text(data=coverType.cnt, aes(label=paste0(round(pct*100,1),"%"),y=count+6500),size=1) +
    theme_adMobile()
  
  fig_nums("countCT","Distribution of coverType")
  
  #Call Bar plot
  plot+ggtitle(eval(fig_nums("countCT")))
  
# Cover_Type in the dataset is extremely disproportionate distributed. Here two classes (1.SpruceFir, 2.LodgepolePine)
#comprise 85% of all rows. 
  
############################   
# Numerical Data Exploration  
############################  
  
  #Create denstiy plot
  
  featuresToPlot<-c("Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire","HS.9am","HS.noon","HS.3pm")
  p=list()
  
  for(i in 1:length(featuresToPlot)){
    p[[i]] <- ggplot(coverData1, aes_string(x=featuresToPlot[i])) + 
      geom_density() + 
      theme_adMobile() + 
      theme(axis.title.y=element_blank())
  }
  fig_nums("denseNumeric","Density Plot of Numeric Data")
  
  #Call density plot
  do.call(grid.arrange,c(p,top=eval(fig_nums("denseNumeric"))))

  #### Density Plots & Bar Plots
  
# Elevation has a relative normal distribution, although it skewed to the left
#   
# Aspect has a bimodal distribution 
#     
# Slope, HD.Hyrdo, HD.Road have similar distribution where it's skewed to the right
#   
# HS.9an and HS.noon distributions display left skew, normal.
#   
# VD.Hyrdo is peaked around 0  
  
  

  #Create box plot
  
  p=list()
  for(i in 1:length(featuresToPlot)){
    p[[i]] <- ggplot(coverData1, aes_string(y=featuresToPlot[i],x="coverType")) + 
      geom_boxplot(outlier.shape = NA) + 
      theme_adMobile() +
      theme(axis.text.x=element_blank(),axis.title.x=element_blank())
    
  }
  fig_nums("boxNumeric","Boxplot of Numeric Data - coverType")
  
  #call boxplot
  do.call(grid.arrange,c(p,top=eval(fig_nums("boxNumeric"))))


# The 10 numeric features are also examined in conjuction with the 7 levels of the **coverType** varaible. 
  
# Outlier points not included
    
# Elevation appears to have the biggest class distinctions
  
    
  
  # Correlation plot
  
  plot<-ggcorr(coverData1[,1:10,with=FALSE], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE, hjust = 00.75, size = 3,layout.exp = 1)
  fig_nums("corrTen","Correlation Matrix of 10 Numerical Features")
  
  #call correlation plot
  plot+ggtitle(eval(fig_nums("corrTen")))

#   A correlation matrix of the 10 numerical variables is created and plotted 
#   
#   There are six pairwise correlations that have a value higher than absolute 0.5
#   
#   HS.noon, HS.3pm (0.59)
#   HD.9am, HS.3pm (-0.78)
#   HD.Hyrdro, VD.Hyrdo (0.61)
#   Slope, HS.noon (-0.61)
#   Aspect, HS.9am (-0.58)
#   Aspect, HS.3pm (0.65)

  #set scatter plot numeric variables
  
  corrFeature1<-c("HS.noon","HS.9am","HD.Hydro","Slope","Aspect","Aspect","Elevation")
  corrFeature2<-c("HS.3pm","HS.3pm","VD.Hydro","HS.noon","HS.9am","HS.3pm","HD.Road")
  
  #create sample since there are too many data point
  scatterTemp<-sample_n(coverData1,50000)
  
  
  # Scatter plot 
  p=list()
  for(i in 1:length(corrFeature1)){
    p[[i]] <- ggplot(scatterTemp, aes_string(x=corrFeature1[i],y=corrFeature2[i])) +
      geom_point(alpha=1/10) +
      theme_adMobile()  
  }
  fig_nums("scatterPlot7","Scatterplot of 7 Correlated Features")
  
  #call scatter plot
  do.call(grid.arrange,c(p,ncol=2,top=eval(fig_nums("scatterPlot6"))))

  
# The hillshade at noon and 3pm creates an elipsoid.
#   
# As the horizontal distance to a hydro increases, the variance in vertical distance increases. 
#   
# As slope increase a proportion othe data's hillshade at noon decreases
# 
# H3.3pm has a sigmoid relationship with Aspect
# 
# Aspect and HS.9am have a more difined sigmoid relationship. 
  

#Categorical Data Exploration
  
  #Bar Chart by wildernessArea and SoilType
  
  categoryCount <- coverData1 %>%group_by(wildernessArea) %>% summarise(count=n())
  p1 <- ggplot(categoryCount, aes(x=reorder(wildernessArea, -count), y=count)) + 
    geom_bar(stat="identity") +
    xlab("wildernesArea") +
    scale_y_continuous(labels=comma) +
    theme_adMobile() + 
    theme(axis.title.y=element_blank())
  
  categoryCount <- coverData1 %>%group_by(soilType) %>% summarise(count=n())
  p2 <- ggplot(categoryCount, aes(x=reorder(soilType, -count), y=count)) + 
    geom_bar(stat="identity") +
    xlab("soilType") +
    scale_y_continuous(labels=comma) +
    theme_adMobile() +
    theme(axis.text.x=element_text(size=6),axis.title.y=element_blank()) 
  
  fig_nums("category2","Row Counts of Categorioal Features")
  
  #call scatter plot
  grid.arrange(p1,p2,ncol=1,top=eval(fig_nums("category4")))
  
# Feature Importance
  
  #Random Forest
  
  #Convert cateogry variables to factors
  
  coverData1$coverType = as.factor(coverData1$coverType)
  coverData1$soilType = as.factor(coverData1$soilType)
  coverData1$wildernessArea = as.factor(coverData1$wildernessArea)
  
  require('caret')
  require('randomForest')
  
  #Use a sample of the data for the rf calculation as it takes long time. 
  sampleIndex <- createDataPartition(coverData1$coverType, p = .1,list = FALSE,times = 1)
  
  #fit <- randomForest(coverType~., coverData1[ sampleIndex,], importance=TRUE)
  rf<- randomForest(coverType~. ,coverData1[ sampleIndex,], importance.type = 1)
  
  # accuracy, discrete target
  importance(rf)
  varImpPlot(rf,type=2)
  
  #Elevation has the higest imporatance, follow by soiltype and HD.Road. 
  
  #Boruta
  library(Boruta)
  
  train <- coverData1[sampleIndex,]

  train_Boruta <- train[ , colnames(train[,13,with=FALSE]):=NULL]

  bor.results <- Boruta(train_Boruta, train$coverType,
                        maxRuns=200)
  
  plot(bor.results)
  names(bor.results)
  bor.results$finalDecision
  CONFIRMED_VAR <- getSelectedAttributes(bor.results)
  Boruta_output <- attStats(bor.results)
  
  
  
  
  
