# Try Neural Network
# Start from SVM dataset


nn.covtype.train_STD.down <- covtype.train_STD.down

nn.covtype.train_STD.down$Wilderness_Area1 <- as.numeric(nn.covtype.train_STD.down$Wilderness_Area1)
nn.covtype.train_STD.down$Wilderness_Area2 <- as.numeric(nn.covtype.train_STD.down$Wilderness_Area2)
nn.covtype.train_STD.down$Wilderness_Area3 <- as.numeric(nn.covtype.train_STD.down$Wilderness_Area3)
nn.covtype.train_STD.down$Wilderness_Area4 <- as.numeric(nn.covtype.train_STD.down$Wilderness_Area4)
nn.covtype.train_STD.down$aquolis <- as.numeric(nn.covtype.train_STD.down$aquolis)
nn.covtype.train_STD.down$bross <- as.numeric(nn.covtype.train_STD.down$bross)
nn.covtype.train_STD.down$bullwark <- as.numeric(nn.covtype.train_STD.down$bullwark)
nn.covtype.train_STD.down$catamount <- as.numeric(nn.covtype.train_STD.down$catamount)
nn.covtype.train_STD.down$cathedral <- as.numeric(nn.covtype.train_STD.down$cathedral)
nn.covtype.train_STD.down$cryaquepts <- as.numeric(nn.covtype.train_STD.down$cryaquepts)
nn.covtype.train_STD.down$cryaquolis <- as.numeric(nn.covtype.train_STD.down$cryaquolis)
nn.covtype.train_STD.down$cryoborolis <- as.numeric(nn.covtype.train_STD.down$cryoborolis)
nn.covtype.train_STD.down$cryorthents <- as.numeric(nn.covtype.train_STD.down$cryorthents)
nn.covtype.train_STD.down$cryumbrepts <- as.numeric(nn.covtype.train_STD.down$cryumbrepts)
nn.covtype.train_STD.down$gothic <- as.numeric(nn.covtype.train_STD.down$gothic)
nn.covtype.train_STD.down$haploborolis <- as.numeric(nn.covtype.train_STD.down$haploborolis)
nn.covtype.train_STD.down$legault <- as.numeric(nn.covtype.train_STD.down$legault)
nn.covtype.train_STD.down$leighcan <- as.numeric(nn.covtype.train_STD.down$leighcan)
nn.covtype.train_STD.down$limber <- as.numeric(nn.covtype.train_STD.down$limber)
nn.covtype.train_STD.down$moran <- as.numeric(nn.covtype.train_STD.down$moran)
nn.covtype.train_STD.down$ratake <- as.numeric(nn.covtype.train_STD.down$ratake)
nn.covtype.train_STD.down$rogert <- as.numeric(nn.covtype.train_STD.down$rogert)
nn.covtype.train_STD.down$troutville <- as.numeric(nn.covtype.train_STD.down$troutville)
nn.covtype.train_STD.down$unspecified.x <- as.numeric(nn.covtype.train_STD.down$unspecified.x)
nn.covtype.train_STD.down$vanet <- as.numeric(nn.covtype.train_STD.down$vanet)
nn.covtype.train_STD.down$bouldery <- as.numeric(nn.covtype.train_STD.down$bouldery)
nn.covtype.train_STD.down$extremely <- as.numeric(nn.covtype.train_STD.down$extremely)
nn.covtype.train_STD.down$normal <- as.numeric(nn.covtype.train_STD.down$normal)
nn.covtype.train_STD.down$rubbly <- as.numeric(nn.covtype.train_STD.down$rubbly)
nn.covtype.train_STD.down$unspecified.y <- as.numeric(nn.covtype.train_STD.down$unspecified.y)
nn.covtype.train_STD.down$very <- as.numeric(nn.covtype.train_STD.down$very)
nn.covtype.train_STD.down$alpine <- as.numeric(nn.covtype.train_STD.down$alpine)
nn.covtype.train_STD.down$lower_montane <- as.numeric(nn.covtype.train_STD.down$lower_montane)
nn.covtype.train_STD.down$montane <- as.numeric(nn.covtype.train_STD.down$montane)
nn.covtype.train_STD.down$montane_and_subalpine <- as.numeric(nn.covtype.train_STD.down$montane_and_subalpine)
nn.covtype.train_STD.down$montane_dry <- as.numeric(nn.covtype.train_STD.down$montane_dry)
nn.covtype.train_STD.down$montane_dry_and_montane <- as.numeric(nn.covtype.train_STD.down$montane_dry_and_montane)
nn.covtype.train_STD.down$subalpine <- as.numeric(nn.covtype.train_STD.down$subalpine)
nn.covtype.train_STD.down$alluvium <- as.numeric(nn.covtype.train_STD.down$alluvium)
nn.covtype.train_STD.down$glacial <- as.numeric(nn.covtype.train_STD.down$glacial)
nn.covtype.train_STD.down$igneous_and_metamorphic <- as.numeric(nn.covtype.train_STD.down$igneous_and_metamorphic)
#nn.covtype.train_STD.down$mixed_sedimentary <- as.numeric(nn.covtype.train_STD.down$mixed_sedimentary)


# nn.covtype.train_STD.down$C1 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==1,1,0)
# nn.covtype.train_STD.down$C2 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==2,1,0)
# nn.covtype.train_STD.down$C3 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==3,1,0)
# nn.covtype.train_STD.down$C4 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==4,1,0)
# nn.covtype.train_STD.down$C5 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==5,1,0)
# nn.covtype.train_STD.down$C6 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==6,1,0)
# nn.covtype.train_STD.down$C7 <- ifelse(nn.covtype.train_STD.down$Cover_Type_Numeric==7,1,0)


nn.covtype.train_STD.down$C1 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Spruce/Fir',1,0)
nn.covtype.train_STD.down$C2 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Lodgepole Pine',1,0)
nn.covtype.train_STD.down$C3 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Ponderosa Pine',1,0)
nn.covtype.train_STD.down$C4 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Cottonwood/Willow',1,0)
nn.covtype.train_STD.down$C5 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Aspen',1,0)
nn.covtype.train_STD.down$C6 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Douglas-fir',1,0)
nn.covtype.train_STD.down$C7 <- ifelse(nn.covtype.train_STD.down$Cover_Type=='Krummholz',1,0)


nn.covtype.train_STD.down <- nn.covtype.train_STD.down[,c(1:14,59:95,97:103)]
# 
# 
# m <- model.matrix( 
#   ~ nn.covtype.train.down2$Wilderness_Area +  
#     nn.covtype.train.down2$aquolis.f +  
#     nn.covtype.train.down2$bross.f +  
#     nn.covtype.train.down2$bullwark.f +  
#     nn.covtype.train.down2$catamount.f +  
#     nn.covtype.train.down2$cathedral.f +  
#     nn.covtype.train.down2$cryaquepts.f +  
#     nn.covtype.train.down2$cryaquolis.f +  
#     nn.covtype.train.down2$cryoborolis.f +  
#     nn.covtype.train.down2$cryorthents.f +  
#     nn.covtype.train.down2$cryumbrepts.f +  
#     nn.covtype.train.down2$gothic.f +  
#     nn.covtype.train.down2$haploborolis.f +  
#     nn.covtype.train.down2$legault.f +  
#     nn.covtype.train.down2$leighcan.f +  
#     nn.covtype.train.down2$limber.f +  
#     nn.covtype.train.down2$moran.f +  
#     nn.covtype.train.down2$ratake.f +  
#     nn.covtype.train.down2$rogert.f +  
#     nn.covtype.train.down2$troutville.f +  
#     nn.covtype.train.down2$unspecified.x.f +  
#     nn.covtype.train.down2$vanet.f +  
#     nn.covtype.train.down2$lower_montane.f +  
#     nn.covtype.train.down2$montane.f +  
#     nn.covtype.train.down2$montane_and_subalpine.f +  
#     nn.covtype.train.down2$montane_dry.f +  
#     nn.covtype.train.down2$montane_dry_and_montane.f +  
#     nn.covtype.train.down2$subalpine.f +  
#     nn.covtype.train.down2$alluvium.f +  
#     nn.covtype.train.down2$glacial.f +  
#     nn.covtype.train.down2$igneous_and_met.f +  
#     nn.covtype.train.down2$stoniness +  
#     nn.covtype.train.down2$has_stoniness +  
#     nn.covtype.train.down2$rock_outcroup +  
#     nn.covtype.train.down2$rock_land +  
#     nn.covtype.train.down2$Aspect_STD +  
#     nn.covtype.train.down2$Hillshade_3pm_STD +  
#     nn.covtype.train.down2$Hillshade_9am.cube_STD +  
#     nn.covtype.train.down2$Hillshade_Noon.cube_STD +  
#     nn.covtype.train.down2$Horizontal_Distance_To_Fire_Points.sqrt_STD +  
#     nn.covtype.train.down2$Horizontal_Distance_To_Roadways.sqrt_STD +  
#     nn.covtype.train.down2$Slope.sqrt_STD +  
#     nn.covtype.train.down2$HV_Distance_To_Hydrology_STD +  
#     nn.covtype.train.down2$mean_hillshade_STD +  
#     nn.covtype.train.down2$Elevation.sqr_STD +  
#     nn.covtype.train.down2$C1 +  
#     nn.covtype.train.down2$C2 +  
#     nn.covtype.train.down2$C3 +  
#     nn.covtype.train.down2$C4 +  
#     nn.covtype.train.down2$C5 +  
#     nn.covtype.train.down2$C6 +  
#     nn.covtype.train.down2$C7  
#     , 
#   data = nn.covtype.train.down2 
# )
# head(m)


######################################################
library(neuralnet)
tm_start<-Sys.time()
set.seed(1231239)
nn<-neuralnet(nn.covtype.train_STD.down$C1 +  
                nn.covtype.train_STD.down$C2 +  
                nn.covtype.train_STD.down$C3 +  
                nn.covtype.train_STD.down$C4 +  
                nn.covtype.train_STD.down$C5 +  
                nn.covtype.train_STD.down$C6 +  
                nn.covtype.train_STD.down$C7  
                ~ 
                 nn.covtype.train_STD.down$Elevation +  
                 nn.covtype.train_STD.down$Aspect +  
#                 nn.covtype.train_STD.down$Slope +  
                 nn.covtype.train_STD.down$Horizontal_Distance_To_Hydrology +  
                 nn.covtype.train_STD.down$Vertical_Distance_To_Hydrology +  
                 nn.covtype.train_STD.down$Horizontal_Distance_To_Roadways +  
                 nn.covtype.train_STD.down$Hillshade_9am +  
                 nn.covtype.train_STD.down$Hillshade_Noon +  
                 nn.covtype.train_STD.down$Hillshade_3pm +  
                 nn.covtype.train_STD.down$Horizontal_Distance_To_Fire_Points +  
#                 nn.covtype.train_STD.down$Wilderness_Area1 +  
#                  nn.covtype.train_STD.down$Wilderness_Area2 +  
#                  nn.covtype.train_STD.down$Wilderness_Area3 +  
#                  nn.covtype.train_STD.down$Wilderness_Area4 +  
  #                 nn.covtype.train_STD.down$aquolis +  
#                 nn.covtype.train_STD.down$bross +  
#                 nn.covtype.train_STD.down$bullwark +  
#                 nn.covtype.train_STD.down$catamount +  
#                 nn.covtype.train_STD.down$cathedral +  
#                 nn.covtype.train_STD.down$cryaquepts +  
#                 nn.covtype.train_STD.down$cryaquolis +  
#                 nn.covtype.train_STD.down$cryoborolis +  
#                 nn.covtype.train_STD.down$cryorthents +  
#                 nn.covtype.train_STD.down$cryumbrepts +  
#                 nn.covtype.train_STD.down$gothic +  
#                 nn.covtype.train_STD.down$haploborolis +  
#                 nn.covtype.train_STD.down$legault +  
                 nn.covtype.train_STD.down$leighcan +  
#                 nn.covtype.train_STD.down$limber +  
                 nn.covtype.train_STD.down$moran +  
#                 nn.covtype.train_STD.down$ratake +  
#                 nn.covtype.train_STD.down$rogert +  
#                 nn.covtype.train_STD.down$troutville +  
#                 nn.covtype.train_STD.down$unspecified.x +  
#                 nn.covtype.train_STD.down$vanet +  
                  nn.covtype.train_STD.down$bouldery +  
                  nn.covtype.train_STD.down$extremely +  
                  nn.covtype.train_STD.down$normal +  
                  nn.covtype.train_STD.down$rubbly +  
                  nn.covtype.train_STD.down$unspecified.y +  
                 nn.covtype.train_STD.down$very +  
#                 nn.covtype.train_STD.down$alpine +  
                 nn.covtype.train_STD.down$lower_montane +  
                 nn.covtype.train_STD.down$montane +  
#                 nn.covtype.train_STD.down$montane_and_subalpine +  
#                 nn.covtype.train_STD.down$montane_dry +  
#                 nn.covtype.train_STD.down$montane_dry_and_montane +  
                 nn.covtype.train_STD.down$subalpine +  
#                 nn.covtype.train_STD.down$alluvium +  
#                 nn.covtype.train_STD.down$glacial +  
                 nn.covtype.train_STD.down$igneous_and_metamorphic   
             
              , data=nn.covtype.train_STD.down
              , linear.output=FALSE
              , hidden=1
              , lifesign="full"
              #, rep=3
              , err.fct="ce"
              #, err.fct="sse"
              , stepmax=500000
              , learningrate = 0.50
)
tm_start
Sys.time()
#283000	min thresh: 0.0101759247
#283394	error: 3507.42809	time: 11.7 mins
#data.frame(nn$result.matrix[c(1:3),c(1:3)])
data.frame(nn$result.matrix)

print(nn)
#Plot the neural network
plot(nn)

covtype.test_STD_S <- covtype.test_STD[,c(1,2,4:10,28,30,36:41,43,44,48,51)]
#Test the neural network 
nn.results<-compute(nn,covariate=covtype.test_STD_S) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(nn.results)

#Lets see the results
print(round(nn.results$net.result))
# tmp<-as.data.frame(round(nn.results$net.result))
# tmp$class_id <- ifelse(tmp$V1==1,1,0)
# tmp$class_id <- ifelse(tmp$V2==1,2,tmp$class_id)
# tmp$class_id <- ifelse(tmp$V2==1,3,tmp$class_id)

#nn.results$net.result[61,]

tmp2<-as.data.frame(nn.results$net.result)
tmp2$maxcol<-colnames(tmp2)[max.col(tmp2,ties.method="first")]
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V1",1,0)
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V2",2,tmp2$Cover_Type_pred)
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V3",3,tmp2$Cover_Type_pred)
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V4",4,tmp2$Cover_Type_pred)
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V5",5,tmp2$Cover_Type_pred)
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V6",6,tmp2$Cover_Type_pred)
tmp2$Cover_Type_pred<-ifelse(tmp2$maxcol=="V7",7,tmp2$Cover_Type_pred)


covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C1==1,1,0)
covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C2==1,2,covtype.test_STD$Cover_Type)
covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C3==1,3,covtype.test_STD$Cover_Type)
covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C4==1,4,covtype.test_STD$Cover_Type)
covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C5==1,5,covtype.test_STD$Cover_Type)
covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C6==1,6,covtype.test_STD$Cover_Type)
covtype.test_STD$Cover_Type<-ifelse(covtype.test_STD$C7==1,7,covtype.test_STD$Cover_Type)
covtype.test_STD$Cover_Type = as.factor(covtype.test_STD$Cover_Type)
tmp2$Cover_Type_pred = as.factor(covtype.test_STD$Cover_Type)

confusionMatrix(tmp2$Cover_Type_pred, covtype.test_STD$Cover_Type, positive = levels(covtype.test_STD$Cover_Type)[7])
#Accuracy : 0.3896437   

write.csv(tmp2, file="C:/Stuff/Northwestern/Classes/2016.1 Winter PREDICT454/Assignments/Assignment 4/tmp2.csv",row.names=TRUE)
write.csv(wine.data, file="C:/Stuff/Northwestern/Classes/2016.1 Winter PREDICT454/Assignments/Assignment 4/wine.data.tmp.csv",row.names=TRUE)


