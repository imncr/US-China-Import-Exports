library(ggplot2)
library(forecast)
library(fpp2)
library(ggTimeSeries)

a=Combined.US.China.ImportExport

month = a[,1]
imports = a[,3]
exports = a[,2]
balance = a[,4]

#creating time series with three columns
data=ts(cbind(imports, exports, balance),start=c(1985,1),frequency=12)
data

#visual comparison of time series
autoplot(data,facets=T)+geom_smooth()+
  ggtitle("Comparison among imports, exports, and trade balance")

#making indiv ts objects for each of the three - cut off at 2000 because prior was not useful info
imports.ts = ts(imports,start=c(2000,1),frequency=12)
exports.ts = ts(exports,start=c(2000,1),frequency=12)
balance.ts = ts(balance ,start=c(2000,1),frequency=12)


#---Decomposing it and plotting the decomposition----#
dec.imports=decompose(imports.ts)
dec.exports=decompose(exports.ts)
dec.balance=decompose(balance.ts)

#plotting additive decompositions
plot(dec.imports)
plot(dec.exports)
plot(dec.balance)

#Trying Multiplicative Decomposition
plot(decompose(imports.ts, type = "mult"))
plot(decompose(exports.ts, type = "mult"))
plot(decompose(balance.ts, type = "mult"))

#---Now creating the lag plot----#
gglagplot(imports.ts,lags=25,set.lags = 1:25)
gglagplot(exports.ts,lags=25,set.lags = 1:25)
gglagplot(balance.ts,lags=25,set.lags = 1:25)

#----Now creating an ACF plot---#
ggAcf(imports,lag.max = 25)+ggtitle("ACF plot for imports")
ggAcf(exports,lag.max = 25)+ggtitle("ACF plot for exports")
ggAcf(balance,lag.max = 25)+ggtitle("ACF plot for trade balance")


