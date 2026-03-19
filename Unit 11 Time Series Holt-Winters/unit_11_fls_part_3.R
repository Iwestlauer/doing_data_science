#3. Seasonal Trend
library(fpp2) #changed from fpp to fpp2
#Load the data
data("austourists")
# Read about the dataset!
?austourists
# Always plot the data first!
plot(austourists)

# returns a ts object.  
aust = window(austourists,start = 1999, end = 2004)

#fit an additive and multiplicative model
fit1s = hw(aust,seasonal = "additive",h = 40)
fit2s = hw(aust,seasonal = "multiplicative",h = 40)



#Plot the original data
plot(aust,
     main = "Aus Tourists Seasonal Trend and Forecast",
     ylab = "Australian Tourists", 
     xlab = "Year", type = "o", 
     xlim = c(1999, 2014),
     ylim = c(15,70))
legend("topleft", 
       legend = c(
         "Holt-Winters' additive", 
         "Holt-Winters' multiplicative"
       ), 
       col = c(
         "blue", 
         "red"
       ), 
       lty = c(1, 2),
       title = "Smoothing Method")

#add the fitted values from the model (of the training data)
lines(fitted(fit1s),col = "blue", type= "o")
lines(fitted(fit2s), col = "red", type= "o")

#Now add the forecasts (add these one at a time)
lines(fit1s$mean, col = "blue")
lines(fit2s$mean,col = "red")

#Compare the accuracy
accuracy(fit1s,austourists)
accuracy(fit2s,austourists)

#add the actual values to visually compare the forecasts to the actual values. 
points(austourists, type = "o")
