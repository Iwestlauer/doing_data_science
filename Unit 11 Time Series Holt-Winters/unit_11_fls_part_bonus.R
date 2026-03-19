# Fun with a great plotting function / package… Dygraphs!  
#   Utilize the dygraphs library.  Read in both Unit12TimeSeries_Ollivander 
# and _Gregorovitch.csv as two different data frames.  
# They do not have headers, so make sure you account for that.  
# This is a time series of Wands sold over years.
# You don’t have your information in the proper format!  
# In both data sets, you’ll need to first convert the date-like variable to an actual Date class.
# Use the library xts (and the xts() function in it) to make each 
# data frame an xts object (effectively, a time series).  
# You’ll want to order.by the Date variable.
# Bind the two xts objects together and create a dygraph from it.  
# Utilize the help() index if you’re stuck.
# Give an effective title and x/y axes.
# Label each Series (via dySeries) to be the appropriate wand-maker.  
# So, one line should create a label for Ollivander and the other for Gregorovitch.
# Stack this graph and modify the two lines to be different colors 
# (and not the default ones!)  Any colors are fine, but make sure they’re 
# visible and that Ollivander is a different color than Gregorovitch.
# Activate a range selector and make it big enough to view.
# Use dyShading to illuminate approximately when Voldemort was 
# revived and at-large: between 1995 to 1999.
# Enable Highlighting on the graph, so mousing over a line bolds it.
