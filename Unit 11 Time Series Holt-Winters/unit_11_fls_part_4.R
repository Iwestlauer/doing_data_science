# Temperature Data Using the maxtemp dataset granted by loading the fpp2 package, 

# Utilize SES to predict the next five years of maximum temperatures 
# in Melbourne.  Plot this information, including the prior data, 
# the SES predictions and the forecast.  
# Add the predicted value line across 1990-present as a separate line, preferably blue.  
# So, to review, you should have the data, the predicted value line overlaying it, 
# and a forecast through 2021, all on one plot. 
# Find the AICc and BIC of this fitted model.  You will use that information later.
# Now use a damped Holt’s linear trend to also predict out five years.  
# Make sure initial=“optimal.”  As above, create a similar plot 
# to the one you just completed for the SES model, but use the Holt fit instead.
# Compare the AICc and BIC of the ses() and holt() models.  Which model is better here?
# Calculate and compare the RMSE from the ses() and holt() models.  
# Which one performs better with respect to this metric?

library(fpp2)

data(maxtemp)
# Maximum annual temperatures at Moorabbin Airport, Melbourne
# Maximum annual temperatures at Moorabbin Airport, Melbourne
# Description
# Maximum annual temperatures (degrees Celsius) for Moorabbin Airport, Melbourne. 1971-2016.
# 
# Format
# Annual time series of class ts.
# 
# Source
# Australian Bureau of Meteorology.
# autoplot(maxtemp)

# Convert to Fahrenheit
maxtemp_F <- maxtemp * 9/5 + 32

# Restrict data to 1990–2016
temp <- window(maxtemp_F, start = 1990, end = 2016)
# Always plot the data first! 
plot(temp,
     ylab = "Maximum Temperature (Fahrenheit)", 
     xlab = "Year", 
     main = "Max Temps in Melbourne - SES")

# Fit SES model (automatic alpha selection)
fit <- ses(temp,h = 5)

# Plot the data
plot(temp,
     ylab = "Maximum Temperature (Fahrenheit)",
     xlab = "Year",
     type = "o",
     xlim = c(1990, 2021),
     ylim = c(80,130),
     main = "Max Temps in Melbourne - SES")

# Add predicted (fitted) values line
lines(fitted(fit), col = "blue", type = "o")

# Add forecast through 2021
lines(fit$mean, col = "blue", type = "o")

# Model information
fit$model$aicc
fit$model$bic

#holt linear

fit1h = holt(temp, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 5)
# Model information
fit1h$model$aicc
fit1h$model$bic

# Reset the Plot!
plot(temp,
     ylab = "Maximum Temperature (Fahrenheit)", 
     xlab = "Year", 
     type = "o", 
     xlim = c(1990, 2021),
     ylim = c(80,130), 
     main = "Max Temps in Melbourne- Holt Linear Damped")
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit1h),col = "blue", type= "o") #trend locally linear
lines(fit1h$mean, col = "blue", type= "o") #trend locally linear

# Calculate and compare the RMSE from the ses() and holt() models.  
# RMSE for SES model
accuracy(fit)

# RMSE for Holt model
accuracy(fit1h)