library(fpp2)
data(ausair)
autoplot(ausair) +
  ggtitle("Total Air Passengers in Australia") +
  ylab("Millions")

# only one data point per year?

# Air Transport Passengers Australia
# Description: Total annual air passengers (in millions) 
# including domestic and international aircraft passengers 
# of air carriers registered in Australia. 1970-2016.
# 
# Format: Annual time series of class ts.
# 
# Source: World Bank.
