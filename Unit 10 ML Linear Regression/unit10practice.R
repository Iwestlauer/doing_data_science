# unit 10 practice
library(tidyverse)
library(reshape2)

#Toy data set with miles per gallon versus weight
dfMPG = read.csv("cars.csv")
dfMPG %>% ggplot(aes(y = MPG, x = Weight)) + geom_point() 

# set the yintercept and slope so we can change later to see the effects on the SSE
yint = 65
slope = -0.015

#Create the residual: observed - expected
residuals = dfMPG$MPG - (yint+dfMPG$Weight*slope)

#Look at sum of the residuals ( which is zero for the regression line)
sum(residuals)
#Look at the sum of the squared residuals which is the SSE or SSR
sum(residuals^2)

#Flexible Plotting
startx = 1000
starty = yint + slope * startx
endx = 5000
endy = yint + slope * endx

dfMPG %>% ggplot(aes(Weight,MPG)) + 
  geom_point() + 
  ggtitle("Linear Fit of MPG vs Weight") +
  geom_segment(aes(x = startx, y = starty, xend = endx, yend = endy)) +
  xlim(0,5000)



#Show lm() and estimation of toy example we have been using
fit = lm(MPG~Weight, data = dfMPG)
summary(fit)
#note that here beta_0_hat and beta_1_hat are beta_0 and beta_1
fit$coefficients
#beta_0_hat
fit$coefficients[1]
#beta_1_hat
fit$coefficients[2]

library(plotly)
p <- ggplot(dfMPG, aes(Weight, MPG)) +
  geom_point() +
  geom_smooth(method = "lm")
plot_ly(p)

dfMPG %>%
  mutate(weight_bin = round(Weight, -2)) %>%   # rounds to nearest 100
  group_by(weight_bin) %>%
  summarise(mean_mpg = mean(MPG))

### simulate MPG values at each weight level
numSamples = 10000
sampInterceptHolder = c()
sampSlopeHolder = c()
for (i in 1:numSamples){
  
  simWeight2k  = rnorm(10, 29, 2)
  simWeight25k = rnorm(10, 25.5, 2)
  simWeight3k  = rnorm(10, 22, 2)
  simWeight35k = rnorm(10, 18.5, 2)
  simWeight4k  = rnorm(10, 15, 2)
  simWeight45k = rnorm(10, 11.5, 2)
  simWeight5k  = rnorm(10, 8, 2)
  
  ### make the corresponding weights
  weights = c(rep(2000,10),
              rep(2500,10),
              rep(3000,10),
              rep(3500,10),
              rep(4000,10),
              rep(4500,10),
              rep(5000,10))
  
  ### make dataframe
  dffullSample = data.frame(
    Weight = weights,
    MPG = c(simWeight2k,
            simWeight25k,
            simWeight3k,
            simWeight35k,
            simWeight4k,
            simWeight45k,
            simWeight5k)
  )
  
  dffullSample
  
  ### plot the data
  dffullSample %>% ggplot(aes(x = Weight, y = MPG)) + geom_point()
  
  ### plot the data and the line
  dffullSample %>% ggplot(aes(x = Weight, y = MPG)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    ggtitle("Simulated linear fit based on mean and sd of original data")
  
  ### get the sample intercept and slope
  fit = lm(MPG~Weight, data = dffullSample)

  sampInterceptHolder[i] = fit$coefficients[1]
  sampSlopeHolder[i] = fit$coefficients[2]
}
#Used a simple histogram (rather than from ggplot) since not for presentation
# hist(sampInterceptHolder, 
#      main = str_c("Distribution of Intercepts from ", 
#                   numSampIntercepts, " Samples"), 
#      xlab = "Sample Intercept")
# summary(sampInterceptHolder)

hist(sampSlopeHolder, 
     main = str_c("Distribution of Slopes from ", 
                  numSamples, " Samples"), 
     xlab = "Sample Slope")
summary(sampSlopeHolder)


##################################################
## Now show calculation of t-statistic and pvalue
fit = lm(MPG~Weight, data = dfMPG)
summary(fit)
confint(fit)

#Intercept
tstat = 46.2734424/0.7974987 #beta_0_hat / SE(beta_0_hat)
#dof is 392
# pvalue = (1-pt(tstat,390)) * 2 # this pvalue is smaller than machine precision
pvalue = 2 * pt(abs(tstat), df = 390, lower.tail = FALSE)
tstat # around 58
pvalue # nearly 0

#Slope
tstat = -0.0076613  /0.0002577   #beta_1_hat / SE(beta_1_hat)
# pvalue = (pt(abs(tstat),390)) * 2 # Mult by 2 since 2 sided test
# pvalue = 2 * (1 - pt(abs(tstat), 390))
pvalue = 2 * pt(abs(tstat), df = 390, lower.tail = FALSE)
tstat #around -30
pvalue # 2 ???


alpha = 0.05
mean_weight = mean(dfMPG$Weight)

#####################################################
#internal leave one out cross validation of models of different degrees

#Recall

dfMPG %>% 
  ggplot(aes(x = Weight, y = MPG)) + 
  geom_point() + ggtitle("cars: mpg v. weight") + 
  geom_smooth(method = "lm") + xlim(1000,6000)

# degree 1 linear regression model
fit = lm(MPG~Weight, data = dfMPG)
summary(fit)
#degree 2 linear regression model
dfMPG %>% ggplot(aes(x = Weight, y = MPG)) + 
  geom_point()
dfMPG2 = dfMPG %>% mutate(Weight2 = Weight^2)
fit = lm(MPG~Weight+Weight2, dfMPG2)
summary(fit)

#visualize the quadratic fit
preds = predict(fit)
confidence = confint(fit)
dfMPG %>% ggplot(aes(x = Weight, y = MPG)) + 
  geom_point() +
  geom_line(data = dfMPG, 
            aes( x = Weight, y = preds, col = "red")) +
  ggtitle("Quadratic fit using 2-degree lm")


#PLOT THE ERRORS FOR QUADRATIC FIT
fit = lm(MPG~Weight+Weight2, dfMPG2)
hist(fit$residuals, col = "blue", main = "Histogram of Residuals")
plot(fit$fitted.values,fit$residuals, 
     main = "Plot of Residuals v. Fitted Values",
     sub = "Quadratic lm Model")
library(GGally)
dfMPG %>% select(MPG,Weight) %>% ggpairs()
#PLOT THE ERRORS FOR LINEAR FIT
fit = lm(MPG~Weight, data = dfMPG)
hist(fit$residuals, col = "blue", main = "Histogram of Residuals")
plot(fit$fitted.values,fit$residuals, 
     main = "Plot of Residuals v. Fitted Values",
     sub = "Linear lm Model")
# library(GGally)
dfMPG %>% select(MPG,Weight) %>% ggpairs()


#Leave one out cross-validation
# set up
numMSPEs <- 1000  # number of iterations
MSPEHolderModel1 <- numeric(numMSPEs)
MSPEHolderModel2 <- numeric(numMSPEs)
for (i in 1:numMSPEs) {
  
  # Randomly select 75% of rows for training
  TrainObs <- sample(seq(1, nrow(dfMPG)), round(0.75 * nrow(dfMPG)), replace = FALSE)
  TrainData <- dfMPG[TrainObs, ]
  TestData <- dfMPG[-TrainObs, ]
  
  # --- Model 1: Linear ---
  Model1_fit <- lm(MPG ~ Weight, data = TrainData)
  Model1_Preds <- predict(Model1_fit, newdata = TestData)
  MSPEHolderModel1[i] <- mean((TestData$MPG - Model1_Preds)^2)
  
  # --- Model 2: Quadratic ---
  Model2_fit <- lm(MPG ~ Weight + Weight2, data = dfMPG2[TrainObs, ])
  Model2_Preds <- predict(Model2_fit, newdata = dfMPG2[-TrainObs, ])
  MSPEHolderModel2[i] <- mean((dfMPG2$MPG[-TrainObs] - Model2_Preds)^2)
}
mean(MSPEHolderModel1)  # Average MSPE for linear
mean(MSPEHolderModel2)  # Average MSPE for quadratic

boxplot(MSPEHolderModel1, MSPEHolderModel2,
        names = c("Linear", "Quadratic"),
        main = "Cross-Validated MSPEs")

#############################################################
# impute the missing horsepowers by using linear regression
dfMPG = read.csv("cars.csv")
sum(is.na(dfMPG$Horsepower)) # two values are missing
train_data <- dfMPG[!is.na(dfMPG$Horsepower), ]
missing_data <- dfMPG[is.na(dfMPG$Horsepower), ]
hp_model <- lm(Horsepower ~ Weight + Displacement + Cylinders, data = train_data)
summary(hp_model)
predicted_hp <- predict(hp_model, newdata = missing_data)
dfMPG$Horsepower[is.na(dfMPG$Horsepower)] <- predicted_hp
sum(is.na(dfMPG$Horsepower)) # no values are missing
##############################################################
# assess the relationship between mpg and horespower
dfMPG %>% ggplot(aes(y = MPG, x = Horsepower)) + 
  geom_point() +
  ggtitle("MPG v. Horsepower")

#degree 2 linear regression model
dfMPG2 = dfMPG %>% mutate(Horsepower2 = Horsepower^2)
fit = lm(MPG~Horsepower+Horsepower2, dfMPG2)
summary(fit)
confint(fit)

#visualize the quadratic fit
preds = predict(fit)
confidence = confint(fit)
dfMPG %>% ggplot(aes(x = Horsepower, y = MPG)) + 
  geom_point() +
  geom_line(data = dfMPG, 
            aes( x = Horsepower, y = preds, col = "red")) +
  ggtitle("Quadratic fit using 2-degree lm")




