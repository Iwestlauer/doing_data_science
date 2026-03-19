#2 Holt's Linear Trend Model for AUS AIR
library(fpp2) #changed from fpp to fpp2
# 1. SES MODEL FOR AUS AIR 
data(ausair)

#returns a ts object
air = window(ausair, start = 1990, end = 2004)




fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 5) #trend locally linear
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 5)
fit3h = holt(air, alpha = .8, beta = .2, initial = "optimal", h = 5)
# fit4h = holt(air, alpha = .8, beta = .2, initial = "optimal", exponential = TRUE, h = 5)

# fit5h = holt(air, alpha =0.8, beta = .4, initial = "simple", h = 5)
# fit6h = holt(air, alpha =0.8, beta = .8, initial = "simple", h = 5)


# # Check out estiamted values of the "training" data from the first holt model 
# fitted(fit1h)
# # Check out the forecast value (h of them)
# fit1h$mean

# Reset the Plot!
plot(air,
     ylab = "Airline Passengers", 
     xlab = "Year", 
     main = "Fitted Holt models with different initial and exponential args",
     type = "o", 
     xlim = c(1990, 2009),
     ylim = c(15,60))
legend("topleft", 
       legend = c(
         "simple-linear", 
         "simple-exponential", 
         "optimal-linear"
         # ,"optimal-exponential"
         ), 
       col = c(
               # "red", 
               "blue", 
               "olivedrab", 
               # "purple",
               "coral"
               # ,"yellow"
               ), 
       lty = c(1, 2),
       title = "initial-exponential")
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit1h),col = "blue", type= "o") #trend locally linear
lines(fitted(fit2h), col = "olivedrab", type= "o")
lines(fitted(fit3h), col = "coral", type= "o")
# lines(fitted(fit4h), col = "yellow", type= "o")

#Plot each models forecasts (Do these one by one to see the differences)
lines(fit1h$mean, col = "blue", type= "o") #trend locally linear
lines(fit2h$mean,col = "olivedrab", type= "o")
lines(fit3h$mean,col = "coral", type= "o")
# lines(fit4h$mean,col = "yellow", type= "o")
air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")










# Fit another model ... damped!  
fit3h = holt(air, 
             alpha = .8, 
             beta = .2, 
             damped = TRUE, 
             initial = "optimal", 
             h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit3h), col = "darkgreen", type= "o")
# Plot the forecasts
lines(fit3h$mean,col = "darkgreen", type= "o")

# Fit another model ... what is the difference?  
fit4h = holt(air, 
             alpha = .8, 
             beta = .2, 
             damped = TRUE, 
             initial = "optimal", exponential = TRUE, h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit4h), col = "cyan", type= "o")
#Plot the forecasts
lines(fit4h$mean,col = "cyan", type= "o")

# with implicit Test set... it figures out by the time which are training and which are test. 
accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)

#with explicit Test set ... (same output)
airTest = window(ausair, start = 2005)
accuracy(fit1h, airTest)
accuracy(fit2h, airTest)
accuracy(fit3h, airTest)

#Add the actual values to visually compare forecasts to actual values
air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")
