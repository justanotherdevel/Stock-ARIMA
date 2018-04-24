library (quantmod)
library (tseries)
library (timeSeries)
library (forecast)
library (xts)

getSymbols('GOOG', from='2012-01-01', to='2018-01-01')
stockPrices = GOOG[,4]

stock = diff(log(stockPrices), lag=32)
stock = stock[!is.na(stock)]

plot(stock, type='l', main='log returns plot')

print(adf.test(stock))

# Splits into training and testing 66% and 33%
breakpoint = floor(nrow(stock)*(2.9/3))

# Apply ACF and PACF function to get the plot that'll help in lag value
par(mfrow = c(1,1))
acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)

# Initializing an xts object for Actual log returns
ActualSeries = xts(0, as.Date("2017-11-25", "%Y-%m-%d"))

# Initializing a dataframe for the forecasted return series
forecastedSeries = data.frame(Forecasted = numeric())

for (b in breakpoint:(nrow(stock)-1)) {
	stockTrain = stock[1:b, ]
	stockTest = stock[(b+1):nrow(stock), ]

	# Summary of the ARIMA model using the determined (p,d,q) parameters
	fit = arima(stockTrain, order = c(2,0,2), include.mean=FALSE)
	summary(fit)

	# Plotting a acf plot of the residuals
	acf(fit$residuals, main="Residuals plot")

	# Forecasting the log returns
	arima.forecast = forecast(fit, h=1, level=99)
	summary(arima.forecast)

	# Plotting the forecasst
	par(mfrow = c(1,1))
	plot(arima.forecast, main="ARIMA Forecast")

	# Creating a series of forecasted returns for the forecasted period
	forecastedSeries = rbind(forecastedSeries, arima.forecast$mean[1])
	colnames(forecastedSeries) = c("Forecasted")

	# Creating a series of actual returns for the forecasted period
	ActualReturn = stock[(b+1),]
	ActualSeries = c(ActualSeries, xts(ActualReturn))
	rm(ActualReturn)

	print(stockPrices[(b+1),])
	print(stockPrices[(b+2),])
}

# Adjust the length of the Actual return series
ActualSeries = ActualSeries[-1]

# Create a time series object of the forecasted series
forecastedSeries = xts(forecastedSeries, index(ActualSeries))

# Create a plot of the two return series - Actual vs Forecasted
plot (ActualSeries, type='l', main='Actual Returns vs Forecasted Returns')
lines(forecastedSeries, lwd=1.5, col='red')
legend('bottomright', c("Actual", "Forecasted"), lty=c(1,1), lwd=c(1.5,1.5), col=c('black', 'red'))

# Create a table for the accuracy of the forecast
comparision = merge(ActualSeries, forecastedSeries)
comparision$Accuracy = sign(comparision$ActualSeries) == sign(comparision$Forecasted)
print(comparision)

# Compute the accuracy percentage metric
AccuracyPercentage = sum (comparision$Accuracy == 1)*100/length(comparision$Accuracy)
message ("Accuracy: ", AccuracyPercentage, "%")
