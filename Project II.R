Project II Script
########## German Inflation Rates Project
https://www.statbureau.org/en/germany/inflation-tables

mydata = scan()

plot.ts(mydata)

germaninfl = ts(mydata, start = 2008, frequency = 12)

plot(germaninfl)

# Seasonal Decomposition
decompose(germaninfl)

plot(decompose(germaninfl))

# Using the stl method
plot(stl(germaninfl, s.window = 7))

library(forecast)

# stl forecasting
plot(stlf(germaninfl, method = "ets"))

# comparison with a standard ets forecast
plot(forecast(ets(germaninfl), h = 24))

# using autoplot
library(ggplot2)
autoplot(stlf(germaninfl, method = "ets"))

## Seasonal Arima (package forecast)
auto.arima(germaninfl, stepwise = T, 
           approximation = F, trace = T)

# Getting an object
germaninflarima = auto.arima(germaninfl, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

# Forecast
forec = forecast(germaninflarima)
plot(forec)

## Exponential Smoothing with ets
# Auto gemerated
ets(germaninfl)
# Forecast plot
germaninflets = ets(germaninfl)

plot(forecast(germaninflets, h = 60))

# Comparison with seasonal Holt Winters model
plot(hw(germaninfl, h = 60))

## Cross Validation of 2 models
germaninflets = ets(germaninfl)
germaninflarima = auto.arima(germaninfl, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

forecastets = function(x, h) {
  forecast(ets(x), h = h)
}

forecastarima = function(x, h) {
  forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
}

etserror = tsCV(germaninfl, forecastets, h=1)
arimaerror = tsCV(germaninfl, forecastarima, h=1)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)

