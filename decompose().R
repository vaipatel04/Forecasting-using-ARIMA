# plot(Year, X1, type = "l")  #a general upward trend
timeseries1 = ts(X1, start = c(1948,1), frequency = 12)
components_ts1 = decompose(timeseries1)
plot(components_ts1)
#observed --> actual data plot
#trend --> general upward movement of the data
#seasonal --> 
#random --> 

# plot(Year, X2, type = "l")
timeseries2 = ts(X2, start = c(1948, 1), frequency = 12)
components_ts2 = decompose(timeseries2)
plot(components_ts2)

# plot(Year, X3, type = "l")
timeseries3 = ts(X3, start = c(1948, 1), frequency = 12)
components_ts3 = decompose(timeseries3)
plot(components_ts3)

# plot(Year, X4, type = "l")
timeseries4 = ts(X4, start = c(1948, 1), frequency = 12)
components_ts4 = decompose(timeseries4)
plot(components_ts4)