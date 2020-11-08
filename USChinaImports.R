#US china imports and exports data
#in millions of dollars

#loading libraries
library(ggplot2)
library(forecast)
library(fpp2)
library(ggTimeSeries)
library(TSclust)
library(prophet)
library(TSEntropies)
library(nonlinearTseries)
library(readxl)

#Preparing the data
IMPCH <- read_excel("Data/IMPCH.xls")
imp <- IMPCH[,2]
imp.ts <- ts(data = imp, start = c(1985,1), frequency = 12)
EXPCH <- read_excel("Data/EXPCH.xls")
exp <- EXPCH[,2]
exp.ts <- ts(data = exp, start = c(1985,1), frequency = 12)
impex.ts <- ts(data = cbind(imp,exp), start = c(1985,1), frequency = 12)
train.imp.ts <- window(imp.ts, end=end(imp.ts)-c(2,0))
train.imp.ts
train.exp.ts <- window(exp.ts, end=end(exp.ts)-c(2,0))
train.exp.ts

#graphing raw data side by side
autoplot(impex.ts, facets = T)
#graphing differenced data side by side
autoplot(diff(impex.ts), facets = T)
#graphing boxcox data
autoplot(cbind(imp.ts,1-(1/imp.ts)), facets = T)
autoplot(cbind(exp.ts,1-(1/exp.ts)), facets = T)

#polar plots
ggseasonplot(imp.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
  ggtitle("Seasonality through polarmap for US Imports of Chinese Goods")
ggseasonplot(exp.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
  ggtitle("Seasonality through polarmap for US Exports to China")

#heatmaps
Time.Stamp = seq(1,nrow(imp),1)
data.imp = cbind(Time.Stamp, imp)
data.exp = cbind(Time.Stamp, exp)
ggplot(data.imp,aes(x = Time.Stamp, y = 1)) +
  geom_tile(aes(fill = Imports)) +
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red", midpoint=28) + ggtitle("Seasonality through heatmap for US Imports of Chinese Goods")+
  ylab("") + scale_y_discrete(expand=c(0,0))
ggplot(data.exp,aes(x = Time.Stamp, y = 1)) +
  geom_tile(aes(fill = Exports)) +
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red", midpoint=28) + ggtitle("Seasonality through heatmap for US Exports to China")+
  ylab("") + scale_y_discrete(expand=c(0,0))

#lag plots
gglagplot(imp.ts,lags=25,set.lags = 1:25)+
  ggtitle("Lag plots, US Imports of Chinese Goods")
gglagplot(exp.ts,lags=25,set.lags = 1:25)+
  ggtitle("Lag plots, US Exports to China")

#checking distance
diss(impex.ts, METHOD = "EUCL")
diss(impex.ts, METHOD = "COR")

#checking outliers
tsoutliers(imp.ts, lambda = "auto")
tsoutliers(exp.ts, lambda = "auto")

#check entropy
SampEn(imp.ts)
SampEn(exp.ts)

#basic decomposition
decomp.imp <- decompose(train.imp.ts, type = "mult")
autoplot(decomp.imp)
decomp.exp <- decompose(train.exp.ts, type = "mult")
autoplot(decomp.exp)

#stl decomposition (Error: only univariate series are allowed)
stl.imp <- stl(train.imp.ts, t.window = 13, s.window="periodic", robust=TRUE)
autoplot(stl.imp)
stl.exp <- stl(train.exp.ts, t.window = 13, s.window="periodic", robust=TRUE)
autoplot(stl.exp)

#ETS
best.ets.imp <- ets(train.imp.ts)
autoplot(best.ets.imp)
accuracy(forecast(best.ets.imp),imp.ts)
checkresiduals(forecast(best.ets.imp))

best.ets.exp <- ets(train.exp.ts)
autoplot(best.ets.exp)
accuracy(forecast(best.ets.exp),exp.ts)
checkresiduals(forecast(best.ets.exp))

#ndiffs
ndiffs(train.imp.ts)
ndiffs(train.exp.ts)

autoplot(train.imp.ts)
autoplot(diff(train.imp.ts))

autoplot(train.exp.ts)
autoplot(diff(train.exp.ts))

#acf and pacf plots
ggAcf(train.imp.ts,lag.max = 25)+ggtitle("ACF plot for US Imports of Chinese Goods data (Raw Data)")
ggPacf(train.imp.ts,lag.max = 25)+ggtitle("PACF plot for US Imports of Chinese Goods data (Raw Data)")

ggAcf(diff(decompose(train.imp.ts)$trend),lag.max = 25)+ggtitle("Trend ACF plot for US Imports of Chinese Goods data (1st diff)")
ggPacf(diff(decompose(train.imp.ts)$trend),lag.max = 25)+ggtitle("Trend PACF plot for US Imports of Chinese Goods data (1st diff)")

ggAcf(diff(decompose(train.imp.ts)$seasonal),lag.max = 25)+ggtitle("Seasonal ACF plot for US Imports of Chinese Goods data (1st diff)")
ggPacf(diff(decompose(train.imp.ts)$seasonal),lag.max = 25)+ggtitle("Seasonal PACF plot for US Imports of Chinese Goods data (1st diff)")

ggAcf(train.exp.ts,lag.max = 25)+ggtitle("ACF plot for US Exports to China data (Raw Data)")
ggPacf(train.exp.ts,lag.max = 25)+ggtitle("PACF plot for US Exports to China Goods data (Raw Data)")

ggAcf(diff(decompose(train.exp.ts)$trend),lag.max = 25)+ggtitle("Trend ACF plot for US Exports to China data (1st diff)")
ggPacf(diff(decompose(train.exp.ts)$trend),lag.max = 25)+ggtitle("Trend PACF plot for US Exports to China Goods data (1st diff)")

ggAcf(diff(decompose(train.exp.ts)$seasonal),lag.max = 25)+ggtitle("Seasonal ACF plot for US Exports to China data (1st diff)")
ggPacf(diff(decompose(train.exp.ts)$seasonal),lag.max = 25)+ggtitle("Seasonal PACF plot for US Exports to China Goods data (1st diff)")

#SARIMA
noseason.arima.imp.nolambda <- auto.arima(train.imp.ts,D=NA,max.q=0,max.P = 0,max.Q = 0,seasonal = F,stepwise = F,trace=T, lambda = NULL)
noseason.arima.imp.yeslambda <- auto.arima(train.imp.ts,D=NA,max.q=0,max.P = 0,max.Q = 0,seasonal = F,stepwise = F,trace=T, lambda = "auto")
summary(noseason.arima.imp.nolambda)
summary(noseason.arima.imp.yeslambda)
accuracy(forecast(noseason.arima.imp.nolambda),imp.ts)
accuracy(forecast(noseason.arima.imp.yeslambda),imp.ts)
checkresiduals(noseason.arima.imp.nolambda)
checkresiduals(noseason.arima.imp.yeslambda)
checkresiduals(forecast(noseason.arima.imp.nolambda))
checkresiduals(forecast(noseason.arima.imp.nolambda))

best.arima.imp.nolambda <- auto.arima(train.imp.ts, stepwise = F, trace = T, lambda = NULL)
best.arima.imp.yeslambda <- auto.arima(train.imp.ts, stepwise = F, trace = T, lambda = "auto")
summary(best.arima.imp.nolambda)
summary(best.arima.imp.yeslambda)
accuracy(forecast(best.arima.imp.nolambda),imp.ts)
accuracy(forecast(best.arima.imp.yeslambda),imp.ts)
checkresiduals(best.arima.imp.nolambda)
checkresiduals(best.arima.imp.yeslambda)
checkresiduals(forecast(best.arima.imp.nolambda))
checkresiduals(forecast(best.arima.imp.yeslambda))

noseason.arima.exp.nolambda <- auto.arima(train.exp.ts,D=NA,max.q=0,max.P = 0,max.Q = 0,seasonal = F,stepwise = F,trace=T, lambda = NULL)
noseason.arima.exp.yeslambda <- auto.arima(train.exp.ts,D=NA,max.q=0,max.P = 0,max.Q = 0,seasonal = F,stepwise = F,trace=T, lambda = "auto")
summary(noseason.arima.exp.nolambda)
summary(noseason.arima.exp.yeslambda)
accuracy(forecast(noseason.arima.exp.nolambda))
accuracy(forecast(noseason.arima.exp.yeslambda))
checkresiduals(noseason.arima.exp.nolambda)
checkresiduals(noseason.arima.exp.yeslambda)
checkresiduals(forecast(noseason.arima.exp.nolambda))
checkresiduals(forecast(noseason.arima.exp.yeslambda))

best.arima.exp.nolambda <- auto.arima(train.exp.ts, stepwise = F, trace = T, lambda = NULL)
best.arima.exp.yeslambda <- auto.arima(train.exp.ts, stepwise = F, trace = T, lambda = "auto")
summary(best.arima.exp.nolambda)
summary(best.arima.exp.yeslambda)
accuracy(forecast(best.arima.exp.nolambda),exp.ts)
accuracy(forecast(best.arima.exp.yeslambda),exp.ts)
checkresiduals(best.arima.exp.nolambda)
checkresiduals(best.arima.exp.yeslambda)
checkresiduals(forecast(best.arima.exp.nolambda))
checkresiduals(forecast(best.arima.exp.yeslambda))

#non-linear testing
nonlinearityTest(train.imp.ts)
nonlinearityTest(train.exp.ts)

#Neural Network
train.imp.ts.boxcox <- BoxCox(train.imp.ts, lambda = BoxCox.lambda(train.imp.ts))
train.exp.ts.boxcox <- BoxCox(train.exp.ts, lambda = BoxCox.lambda(train.exp.ts))

set.seed(100)
neural.imp <- nnetar(train.imp.ts)
neural.imp.boxcox <- nnetar(train.imp.ts.boxcox)
neural.imp
neural.imp.boxcox
accuracy(forecast(neural.imp.boxcox),imp.ts)
accuracy(forecast(neural.imp),imp.ts)
forecast(neural.imp)
forecast(neural.imp.boxcox)

set.seed(100)
neural.exp <- nnetar(train.exp.ts)
neural.exp.boxcox <- nnetar(train.exp.ts.boxcox)
neural.exp
neural.exp.boxcox
accuracy(forecast(neural.exp),exp.ts)
accuracy(forecast(neural.exp.boxcox),exp.ts)
forecast(neural.exp)
forecast(neural.exp.boxcox)

#graphical comparison
autoplot(train.imp.ts)+autolayer(fitted(best.ets.imp))+autolayer(fitted(best.arima.imp.nolambda))+autolayer(fitted(neural.imp))
  autolayer(forecast(best.ets.imp))+autolayer(forecast(best.arima.imp.nolambda))+autolayer(forecast(neural.imp))
autoplot(train.imp.ts)+autolayer(fitted(best.ets.imp))+autolayer(forecast(best.ets.imp))
autoplot(train.imp.ts)+autolayer(fitted(best.arima.imp.nolambda))+autolayer(forecast(best.arima.imp.nolambda))
autoplot(train.imp.ts)+autolayer(fitted(best.arima.imp.yeslambda))+autolayer(forecast(best.arima.imp.yeslambda))
autoplot(train.imp.ts)+autolayer(fitted(neural.imp))+autolayer(forecast(neural.imp))

autoplot(train.exp.ts)+autolayer(fitted(best.ets.exp))+autolayer(fitted(best.arima.exp.nolambda))+autolayer(fitted(neural.exp))
  autolayer(forecast(best.ets.exp))+autolayer(forecast(best.arima.exp.nolambda))+autolayer(forecast(neural.exp))
autoplot(train.exp.ts)+autolayer(fitted(best.ets.exp))+autolayer(forecast(best.ets.exp))
autoplot(train.exp.ts)+autolayer(fitted(best.arima.exp.nolambda))+autolayer(forecast(best.arima.exp.nolambda))
autoplot(train.exp.ts)+autolayer(fitted(best.arima.exp.yeslambda))+autolayer(forecast(best.arima.exp.yeslambda))
autoplot(train.exp.ts)+autolayer(fitted(neural.exp))+autolayer(forecast(neural.exp))

#Bagged Models
set.seed(100)
imp.ts <- ts(data = imp[[1]], start = c(1985,1), end = c(2019,9), frequency = 12)
train.imp.ts <- window(imp.ts, end=end(imp.ts)-c(2,0))
bag.imp <- baggedModel(train.imp.ts, bootstrapped_series = bld.mbb.bootstrap(train.imp.ts, 100))
accuracy(forecast(bag.imp), imp.ts)

set.seed(100)
exp.ts <- ts(data = exp[[1]], start = c(1985,1), end = c(2019,9), frequency = 12)
train.exp.ts <- window(exp.ts, end=end(exp.ts)-c(2,0))
bag.exp <- baggedModel(train.exp.ts, bootstrapped_series = bld.mbb.bootstrap(train.exp.ts, 100, block_size = NULL))
accuracy(forecast(bag.exp), exp.ts)
