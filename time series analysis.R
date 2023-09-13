#Importing the time series library
library(tseries) 

#Importing the csv file 
data=read.csv(file.choose())
values<-data[,-1]
values

#Converting the data into time series data
tsdata<-ts(values,start=c(1960),end=c(2015),frequency=1)
class(tsdata)
ts.plot(tsdata)

#Checking if the time series data is stationary or not 
adf.test(tsdata)

#Importing library necessary for forecasting
library(forecast)

#Fitting an ARIMA model
fit=auto.arima(tsdata,seasonal="FALSE")
fit

#PLotting the Autocorrelation and Partial Autocorrelation plots
acf(diff(tsdata))
pacf(diff(tsdata))

#Residual Analysis
res=resid(fit)
acf(res)

Box.test(res,lag=10,fitdf=1)
shapiro.test(res)

#Out of sample forecast
forecast=forecast(fit,h=12)
forecast
plot(forecast)
