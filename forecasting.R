library(tseries)
library(ggplot2)
library(forecast)
attach(Asst2data)

plot(X1, type = "l") #upward trend --> seems stationary as it moves up at a somewhat constant rate; this has near to constant variance but the mean is changing
plot(X2, type = "l") #nonstationary data
plot(X3, type = "l") #again seems stationary data as it is moving upwards at some rate
plot(X4, type = "l") #nonstationary data
#stationary data --> constant mean and variance


#Model Identification -- what is the order of the ARIMA model?, first lets do a stationary check using augmented Dicky-Fuller test
#p-value > 0.05 implies nonstationarity in the variable
adf.test(X1, alternative = "stationary") #--> more or less stationary, as predicted
adf.test(X2, alternative = "stationary") #--> nonstationary
adf.test(X3, alternative = "stationary") #--> more or less stationary, as predicted
adf.test(X4, alternative = "stationary") #-->nonstationary

#Therfore, according to the p-values, X2 and X4 are the nonstationary time series

##Now that we have determined the time series of all 4 variables, lets estimate the models

acf(X1, 150)#functional form of the data that which determines the order of the AR terms and MA terms in the model; any lag that lies above the blue dotted line is significant
pacf(X1, 150)

acf(X2, 150) 
pacf(X2, 150)

acf(X3, 150)
pacf(X3, 150)

acf(X4, 150)
pacf(X4, 150)

#Stationarity --> difference = 0; therefore we only need to determine difference, d, for X2 and X4
plot.ts(timeseries1)

dX2 <- diff(timeseries2, differences = 1)
plot.ts(dX2)
dX2 <- diff(timeseries2, differences = 2)
plot.ts(dX2)
dX2 <- diff(timeseries2, differences = 3)
plot.ts(dX2)
dX2 <- diff(timeseries2, differences = 10)
plot.ts(dX2)
#difference = 1 strikes more stationary

plot.ts(timeseries3)

dX4 <- diff(timeseries4, differences = 1)
plot.ts(dX4)
dX4 <- diff(timeseries4, differences = 2)
plot.ts(dX4)
dX4 <- diff(timeseries4, differences = 3)
plot.ts(dX4)
dX4 <- diff(timeseries4, differences = 4)
plot.ts(dX4)
#for the most part d=4 looks okay for the X4 difference

#Now we determine the p and q values for our ARIMA model
# acf(timeseries1, lag.max = 130)
# acf(timeseries1, lag.max = 130, plot = "FALSE")
# pacf(timeseries1, lag.max = 130)
# pacf(timeseries1, lag.max = 130, plot = "FALSE")
X1arima <- auto.arima(X1)
X1arima

acf(dX2, lag.max = 20)
acf(dX2, lag.max = 20, plot = "FALSE")
pacf(dX2, lag.max = 20)
pacf(dX2, lag.max = 20, plot = "FALSE")
X2arima <- auto.arima(X2)
X2arima

# acf(timeseries3, lag.max = 130)
# acf(timeseries3, lag.max = 130, plot = "FALSE")
# pacf(timeseries3, lag.max = 130)
# pacf(timeseries3, lag.max = 130, plot = "FALSE")
X3arima <- auto.arima(X3)
X3arima

acf(dX4, lag.max = 20)
acf(dX4, lag.max = 20, plot = "FALSE")
pacf(dX4, lag.max = 20)
pacf(dX4, lag.max = 20, plot = "FALSE")
X4arima <- auto.arima(X4)
X4arima

##Now, that we have estimated the best possible ARIMA model, let us forecast
X1arima <- arima(timeseries1, order = c(2, 1, 1))
X1arima
X1tsforecasts <- forecast(X1arima, h = 5)
X1tsforecasts

X2arima <- arima(timeseries2, order = c(2, 1, 2))
X2arima
X2tsforecasts <- forecast(X2arima, h = 5)
X2tsforecasts

X3arima <- arima(timeseries3, order = c(2, 1, 2))
X3arima
X3tsforecasts <- forecast(X3arima, h = 5)
X3tsforecasts

X4arima <- arima(timeseries4, order = c(1, 1, 1))
X4arima
X4tsforecasts <- forecast(X4arima, h = 5)
X4tsforecasts