rm(list = ls())



install.packages('forecastxgb')

library(zoo)
library(fpp)
library(ggplot2)
library(forecast)
library(fma)
library(TTR)
library(forecastHybrid)
library(prophet)
library(extraDistr)
library(data.table)

#aggregate monthly forecast

 deposit <- read.csv(file="C:/Users/dbing/R Data/Tot_dep_2016.csv", header=TRUE, sep=",", na.strings="?")
# 
# # EDA
# summary(deposit) 
# str(deposit)
# head(deposit)
# class(deposit)
# names(deposit)
# nrow(deposit) # 60
# 
# #Histogram
# hist(deposit[,2], main = "Histogram on Total Deposits")
# 
# #Histogram log transform Total Deposits
# hist(log(deposit[,2]), main = "Histogram on Total Deposits ")
# 
# #Create time series
# #deposit_TS <- ts(deposit$CD, frequency = 12, start = c(2012, 01))
# 
# #Plot Total Deposit
# plot(deposit_TS, ylab="Total Deposit Balance", xlab="Time", main = "MONTH")
# 
# #Plot Total Deposit in Box-Cox transformation, not much change
# lambda = BoxCox.lambda(deposit_TS)
# plot(BoxCox(deposit_TS, lambda), xlab = "Time", ylab = paste("BoxCox(Total Deposit Balance", round(lambda, 2), ")"))
# 
# #Decompose the data
# tsdecompose <- decompose(deposit_TS, type="multiplicative")
# plot(tsdecompose)
# 
# plot(tsdecompose$f)
# 
# # Seasonally adjusted data
# seasAdj <- seasadj(tsdecompose)
# plot(seasAdj, ylab = "Seasonally adjusted data")
# 
# #create plot
# ggtsdisplay(deposit_TS, plot.type="scatter")
# 
# qplot(x=date, y=airt,
#       data=harMetDaily.09.11, na.rm=TRUE,
#       main="Air temperature Harvard Forest\n 2009-2011",
#       xlab="Date", ylab="Temperature (°C)")
# 
# 
# #Simple moving average
# par(mfrow=c(2,2))
# depositTSSMA3 <- SMA(deposit_TS, n=3)
# plot.ts(depositTSSMA3)
# 
# depositTSSMA8<- SMA(deposit_TS, n=8)
# plot.ts(depositTSSMA8)
# 
# #Seasonal Plot
# seasonplot(deposit_TS,ylab="Total Deposit Balance", xlab="Month", 
#            main="Seasonal plot: Total Deposit Balance Plot",
#            year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)
# 
# #Month Plot
# monthplot(deposit_TS, ylab = "log $", xlab = "Month", xaxt = "n", main = "Seasonal Deviation Plot")
# axis(1, at=1:12, labels=month.abb, cex=0.8)
# 
# #Trying different Baseline Models
# 
# plot(rwf(deposit_TS, drift=TRUE, h=12, level=0), xlab="Time", ylab="Value $", main="")
# lines(naive(deposit_TS, h=12, level=0)$mean, xlab="", ylab="", main="", col="green")
# lines(meanf(deposit_TS, h=12, level=0)$mean, xlab="", ylab="", main="", col="red")
# lines(snaive(deposit_TS, h=12, level=0)$mean, xlab="", ylab="", main="", col="orange")
# # "$mean" after the function calls above is used
# # to extract vector of the forecasted values from the function output
# legend("topleft",
#        legend = c("Random walk with drift", "naive", "Mean forecast","Seasonal Naive"),
#        col = c("blue", "green", "red", "orange"), lty=5)
#        
# #Create addtional time series variables
# deposit$DATE = as.Date(strptime(deposit$CYCLE, "%Y%m%d"))
# deposit$DATE_ym = as.Date(as.yearmon(deposit$DATE, '%Ym%'))
# deposit$DATE_m = as.numeric(format(deposit$DATE_ym,"%m"))
# 
# 
# #create outlier method by group by all the month 
# jan <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 1,]
# feb <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 2,]
# mar <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 3,]
# apr <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 4,]
# may <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 5,]
# jun <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 6,]
# jul <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 7,]
# aug <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 8,]
# sep <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 9,]
# oct <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 10,]
# nov <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 11,]
# dec <- deposit[as.numeric(strftime(deposit$DATE_ym, "%m")) %in% 12,]
# 
# #set outlier threshold
# 
# outlier_threshold = 2.5
# 
# jan_top <- mean(jan$TOT) + (outlier_threshold * sd(jan$TOT))
# feb_top <- mean(feb$TOT) + (outlier_threshold * sd(feb$TOT))
# mar_top <- mean(mar$TOT) + (outlier_threshold * sd(mar$TOT))
# apr_top <- mean(apr$TOT) + (outlier_threshold * sd(apr$TOT))
# may_top <- mean(may$TOT) + (outlier_threshold * sd(may$TOT))
# jun_top <- mean(jun$TOT) + (outlier_threshold * sd(jun$TOT))
# jul_top <- mean(jul$TOT) + (outlier_threshold * sd(jul$TOT))
# aug_top <- mean(aug$TOT) + (outlier_threshold * sd(aug$TOT))
# sep_top <- mean(sep$TOT) + (outlier_threshold * sd(sep$TOT))
# oct_top <- mean(oct$TOT) + (outlier_threshold * sd(oct$TOT))
# nov_top <- mean(nov$TOT) + (outlier_threshold * sd(nov$TOT))
# dec_top <- mean(dec$TOT) + (outlier_threshold * sd(dec$TOT))

#deposit_TS <- ts(log(deposit$CD), frequency = 12, start = c(2012, 01))
deposit_TS <- ts((deposit$TOTAL)/1000000, frequency = 12, start = c(2012, 01))


# Roll forward cross validation

fixed.nValid <- 12
fixed.nTrain <- length(deposit_TS) - fixed.nValid
stepsAhead <- 2
error <- rep(0, fixed.nValid - stepsAhead + 1)
percent.error <- rep(0, fixed.nValid - stepsAhead + 1)


for(j in fixed.nTrain:(fixed.nTrain + fixed.nValid - stepsAhead)) {
  train.ts <- window(deposit_TS, start = c(2012, 1), end = c(2012, j))
  valid.ts <- window(deposit_TS, start = c(2012, j + stepsAhead), end = c(2012, j + stepsAhead))
  
  TSLM <- tslm(deposit_TS ~ trend + season)
  TSLMForecast <- forecast(TSLM, h=stepsAhead)

  error[j - fixed.nTrain + 1] <- valid.ts - TSLMForecast$mean[stepsAhead]
  percent.error[j - fixed.nTrain + 1] <- error[j - fixed.nTrain + 1] / valid.ts
}

mean(abs(error))
sqrt(mean(error^2))
mean(abs(percent.error))


#HW Addictive
hw <- hw(trainingSet, seasonal="additive")
hwforcast<- forecast(hw(trainingSet, seasonal="additive"), seasonal="additive",h=stepsAhead)

#Holt-winters Multiplicative
hw2 <- hw(trainingSet, seasonal="multiplicative")
hwforcast2<- forecast(hw2, seasonal="multiplicative",h=3)$mean

#Holt-Winters
hw3 <- HoltWinters(trainingSet, alpha = TRUE, beta = FALSE, gamma = TRUE,
                   seasonal = c("additive", "multiplicative"),
                   start.periods = 2, l.start = NULL, b.start = NULL,
                   s.start = NULL,
                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                   optim.control = list())

hwforcast3<- forecast(hw3, h=3)$mean

#TSLM (time series linear regression)
TSLM <- tslm(trainingSet ~ trend + season)
TSLMForecast <- forecast(TSLM, h=3)$mean

#Train vs Test Accuracy Test
trainingSet = window(deposit_TS, end = c(2016,12))
testSet = window(deposit_TS, start = c(2017,01))

#Try out different models

#Seasonal Naiive

#fit1 <- snaive(trainingSet)
#fit2 <- rwf(trainingSet)
#plot(snaive)

#Check Residual for goodness for fit

#acf(snaive$residuals, lag.max=20, na.action = na.pass)

#Box.test(snaive$residuals, lag=20, type="Ljung-Box")

#plot.ts(snaive$residuals)

#Random Walk
#rwfForecast = rwf(trainingSet, h = 12)$mean

#Random Walk with drift
#rwfWithDriftForecast = rwf(trainingSet, drift = TRUE, h = 12)
#plot(rwfWithDriftForecast)

#Try out different models


#Holt-Winters additive
hw <- hw(trainingSet, seasonal="additive")
hwforcast<- forecast(hw, seasonal="additive",h=3)$mean

#Holt-winters Multiplicative
hw2 <- hw(trainingSet, seasonal="multiplicative")
hwforcast2<- forecast(hw2, seasonal="multiplicative",h=3)$mean

#Holt-Winters
hw3 <- HoltWinters(trainingSet, alpha = TRUE, beta = FALSE, gamma = TRUE,
            seasonal = c("additive", "multiplicative"),
            start.periods = 2, l.start = NULL, b.start = NULL,
            s.start = NULL,
            optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
            optim.control = list())

hwforcast3<- forecast(hw3, h=3)$mean


#hwforecast$mean
#hw$mean

#holt winter additive and multiplicative

#plot(hw,ylab="Total Deposit",
#     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
#lines(fitted(hw), col="red", lty=2)
#lines(fitted(hw2), col="blue", lty=2)
#lines(hw$mean, type="o", col="green")
#lines(hw2$mean, type="o", col="purple")
#lines(testSet, type="o", col="orange")
#lines(trainingSet, type="o", col="yellow")
#legend(2012, 2586750000000, legend=c("hw", "hw2","hwpred","hw2pred","testdata","traindata"),
#       col=c("red", "blue","green","purple","orange","yellow"), lty=1:2, cex=0.6,box.lty=0)

#TSLM (time series linear regression)
TSLM <- tslm(trainingSet ~ trend + season)
TSLMForecast <- forecast(TSLM, h=3)$mean
#plot(TSLM$fitted)
#lines(trainingSet, type="o", col="red")

#STL (Seasonal and Trend decomposition using Loess)
stl <- stl(trainingSet, t.window=60, s.window="periodic", robust=TRUE)
stlForecast <- forecast(stl, method="arima", h=3)$mean
#plot(stlForecast)
#lines(testSet, type="o", col="orange")

#ETS (Error,Trend,Seasonal). 
ETS <- ets(trainingSet)
ETSForecast <- forecast(ETS, method="arima", h=3)$mean
#plot(ETSForecast)
#lines(testSet, type="o", col="orange")

#Neural net
NN <- nnetar(trainingSet, decay=0.005, maxit=10000, p = 5)
NNForecast <- forecast(NN, h=3)$mean

#plot(forecast(NN))
#lines(trainingSet)
#lines(NN$fitted, type="o", col="red")

#NN1 <- NN$fitted
#plot(NNForecast)

#NNForecast1 <- forecast(NN, h=12)
#plot(NNForecast1)

#Neural net2
NN2 <- nnetar(deposit_TS)
NNForecast2 <- forecast(NN2, h=3)$mean

#nerual network

#plot(hw,ylab="Total Deposit",
#     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
#lines(fitted(NN), col="red", lty=2)
#lines(fitted(NN2), col="blue", lty=2)
#lines(NN$mean, type="o", col="green")
#lines(NN2$mean, type="o", col="purple")
#lines(testSet, type="o", col="orange")
#lines(trainingSet, type="o", col="yellow")
#legend(2012, 2486750000000, legend=c("nn", "nn2","nnpred","nn2pred","testdata","traindata"),
#       col=c("red", "blue","green","purple","orange","yellow"), lty=1:2, cex=0.6,box.lty=0)


#NNForecast1 <- forecast(NN, h=12)
#plot(NNForecast1)


#Auto Arima with xreg parameter (Autoregressive Integrated Moving Average models)

#### train for month indicator variables, set up xreg variable
fit_months2 <- seq(as.Date("2012-01-01"), as.Date("2016-12-31"), by="month")
fit_months_df2 <- data.frame(fit_months2)
fit_months_df2$month <- format(fit_months_df2, '%m')
fit_months_df2$year <- format(fit_months_df2, '%Y')
months2 <- data.matrix(fit_months_df2$month)
year2 <- data.matrix(fit_months_df2$year)

#### pred months for indicator variables, set up xreg variable
pred_months <- seq(as.Date("2017-01-01"), as.Date("2017-03-31"), by="month")
pred_months_df <- data.frame(pred_months)
pred_months_df$month <- format(pred_months_df, '%m')
pred_indicator_months <- data.matrix(pred_months_df$month)
pred_months_df$year <- format(pred_months_df, '%Y')
pred_indicator_year <- data.matrix(pred_months_df$year)

##### predicting the hold out set; next 24 months
lambda_BoxCox2 <- BoxCox.lambda(trainingSet)
z2 <- fourier(ts(trainingSet, frequency=12), K=5)
zf2 <- fourier(ts(trainingSet, frequency=12), K=5, h=3)

#Arima with xreg parameter
arimaTrain2 <- auto.arima(trainingSet,  allowdrift=TRUE,xreg=data.frame(z2, months2))
quick_fcast <- forecast(arimaTrain2, h=3, xreg = data.frame(zf2, pred_indicator_months))$mean


#hybridModel
#all methods

hybrid1 <- hybridModel(trainingSet)
hybridforecast1 <- forecast(hybrid1, h=3)

plot(forecast(hybrid1), main = "Forecast from auto.arima, ets, thetam, nnetar, stlm, and tbats model")
class(hybrid1)
plot(hybrid1, type = "fit")
print(hybrid1)

hybrid2 <- hybridModel(trainingSet, models = "aet",
                       weights = "insample.errors", errorMethod = "RMSE")
hybridforecast2 <- forecast(hybrid2, h=3)


#plot(forecast(hybrid2,h=12))
#lines(fitted(hybrid2), col="red", lty=2)
#legend(2012, 3000000000000, legend=c("Actual Data", "Fitted Data","2017 Forecast"),
#       col=c("black", "red","blue"), lty=1:2, cex=0.6,box.lty=0)


#plot(hybrid2,ylab="Total Deposit",
#     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
#lines(fitted(hybrid2), col="red", lty=2)
#lines(hybrid2$mean, type="o", col="green")
#legend(2012, 2686750000000, legend=c("hybrid2", "hybrid3","hybrid2pred","hybrid3pred","testdata"),
#       col=c("red", "blue","green","purple","orange"), lty=1:2, cex=0.6,box.lty=0)


#plot(forecast(hybrid2), main = "Forecast from auto.arima, ets, thetam, nnetar, stlm, and tbats model")

hybrid3 <- hybridModel(trainingSet, models = "aens",
                       a.args = list(max.p = 7, max.q = 7, approximation = FALSE))
hybridforecast3 <- forecast(hybrid3, h=3)


hybrid4<- hybridModel(y = trainingSet, models = "aefnst",
                      a.args = list(max.p = 12, max.q = 12, approximation = FALSE),
                      n.args = list(repeats = 50),
                      s.args = list(robust = TRUE),
                      t.args = list(use.arma.errors = FALSE)) 

hybridforecast4 <- forecast(hybrid4, h=3)



# Prophet

deposit2 <- deposit[1:60,]

history <- data.frame(ds = seq(as.Date('2012-01-01'), as.Date('2016-12-01'), by = 'm'),
                      y = deposit2$TOTAL)

m <- prophet(history)
future <- make_future_dataframe(m, periods = 3,freq = 'month', include_history = FALSE)

forecast <- prophet:::predict.prophet(m, future) 

plot(m, forecast)
prophet_plot_components(m, forecast)

accuracy(hwforcast, testSet)  
accuracy(hwforcast2, testSet)  
accuracy(hwforcast3, testSet)  
accuracy(stlForecast, testSet) 
accuracy(ETSForecast, testSet)
accuracy(NNForecast, testSet) 
accuracy(NNForecast2, testSet) 
accuracy(quick_fcast, testSet) 
accuracy(hybridforecast1, testSet) 
accuracy(hybridforecast2, testSet) 
accuracy(hybridforecast3, testSet) 
accuracy(hybridforecast4, testSet) 
accuracy(forecast$yhat, testSet) 





