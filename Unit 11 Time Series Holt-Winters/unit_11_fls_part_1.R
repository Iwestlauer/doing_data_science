# install.packages("fpp")
library(fpp2) #changed from fpp to fpp2

# 1. SES MODEL FOR AUS AIR 
data(ausair)

#returns a ts object
air = window(ausair, start = 1990, end = 2004)

# Always plot the data first! 
plot(air,ylab = "Airline Passengers", xlab = "Year", main = "Airline Passengers")

#fit 3 different simple exponential smoothing models ... how are they different?
# what does the h paramter do? 
# h: number of periods for forecasting
# alpha: value of smoothing parameter for the level. if null it will be estimated
# initial: method used for selecting initial state values -- simple: the inital 
# values are set to values obtained using simple calculations on the first few observations

fit1 = ses(air, initial = "simple",alpha = .2,h = 3)
fit2 = ses(air,initial = "simple",alpha = .6, h = 3)
fit3 = ses(air, h = 3) #defaults

fit4 = ses(air, initial = "simple",alpha = 0,h = 3)
fit5 = ses(air,initial = "simple",alpha = .6, h = 5)
fit6 = ses(air, h = 10) # default value for non-seasonal-data
fit7 = ses(air)
fit8 = ses(air, initial = "optimal")
fit9 = ses(air, initial = "simple")

# the forecast package has a nice accuracy funciton with various metrics just pass it the 
# the model and the data!  (This is the "training" data)
accuracy(fit1, ausair)
accuracy(fit2, ausair)
accuracy(fit3, ausair)
accuracy(fit4, ausair)
accuracy(fit5, ausair)
accuracy(fit6, ausair)
accuracy(fit7, ausair)
accuracy(fit8, ausair)
accuracy(fit9, ausair)
# accuracy(fit10, ausair)
# accuracy(fit11, ausair)
# accuracy(fit12, ausair)
# accuracy(fit13, ausair)
# accuracy(fit14, ausair)
# accuracy(fit15, ausair)


#Reset the plot
plot(air,
     ylab = "Airline Passengers (millions)", 
     xlab = "Year", 
     type = "o", 
     xlim = c(1990, 2008),
     ylim = c(15,50), 
     main = "Airline Passengers")

#Plot the estimated values from the models .. the "fitted" values are the training values.
lines(fitted(fit1), col = "blue", type = "o")
lines(fitted(fit2), col = "red", type = "o")
lines(fitted(fit3), col = "green", type = "o")

# the  $mean values are the forecasts.
lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")

# These are the actual values!  Compare visually with the forecasts!
air2008 = window(ausair, start = 1990, end = 2007)
points(air2008, type = "o")

# Compare the forecasts with the actual values with various fit metrics.  
accuracy(fit1, air2008)
accuracy(fit2, air2008)
accuracy(fit3, air2008)
