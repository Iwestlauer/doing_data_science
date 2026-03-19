# Fraud and Not Fraud
# Load required libraries for data manipulation, machine learning, and plotting
library(caret)
library(tidyverse)
library(class)
library(pROC)

#Cost Variables # Positive class = Fraud
CostPerFraud = 10000
CostPerFraudInervention = 1000

# Read data from a user-specified CSV file, expecting headers and converting strings to factors
F = read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE)
# Display the first few rows of the dataset to verify its structure
head(F)

#BASELINE KNN CLASSFICATION 
#Unbalanced data with threshhold = .5
# Perform K-nearest neighbors classification; using columns 1 and 2 as predictors and column 3 as the target
#classifications = knn(F[,1:2], F[1:2], F[,3], prob = TRUE, k = 11)

# Scale predictors for KNN (important because amount and time are on different scales)
X = scale(F[, c("PercentSpecChar", "NumWords")])

# Perform K-nearest neighbors classification; using columns 1 and 2 as predictors and column 3 as the target
# Everything classified as not fraud... because of imbalanced data?
classifications = knn(X, X, F[,3], prob = TRUE, k = 5)

# Create a table of predicted vs actual values for assessing classification accuracy
table(classifications, F[,3])
#accuracy = 177/200 = 0.8850
#sensitivity or "recall"= 8/27 = 0.2962
#specificity = 169/173 = 0.9769

# positive = fraud
# true positive = 8 ($8,000)
# false positive = 4 ($4,000)
# true negative = 169 ($0)
# false negative = 19 ($190,000)
# total $202,000
# Compute a confusion matrix to evaluate the accuracy of predictions; using all available measures
CM_F = confusionMatrix(table(classifications, F[,3]), mode = "everything")
# Display the confusion matrix
CM_F

#Cost:
Cost_Base = CostPerFraud*CM_F$table[2] + CostPerFraudInervention*(CM_F$table[1]+CM_F$table[3])
Cost_Base
#NEW THRESHHOLD
# Display the classifications and their attributes; useful for debugging and understanding model output
classifications
attributes(classifications)
#We the need the probability of FRAUD to reclassify with a new threshhold.
# IMPORTANT ... these are the probabilities of the majority class in each neighborhood.(sometimes FRAUD, sometimes NOTFRAUD)
# In the next chunk of code we will use these probabilities to get just the probabililty of FRAUD
attributes(classifications)$prob #Raw probabilities of the majority class that we will standardize to the FRAUD class below



# Compute probabilities specifically for the "FRAUD" class, adjusting based on predicted labels
probs = ifelse(classifications == "FRAUD", attributes(classifications)$prob, 1 - attributes(classifications)$prob)
# Compute probabilities specifically for the "FRAUD" class, adjusting based on predicted labels
probs = ifelse(classifications == "FRAUD", attributes(classifications)$prob, 1 - attributes(classifications)$prob)
classifications[1:200] # Just the lables
probs

# Calculate and display the proportion of "FRAUD" cases in the dataset
summary(F$Label)



probs = ifelse(classifications == "FRAUD",
               attributes(classifications)$prob,
               1-attributes(classifications)$prob)

classifications[1:200]
probs
threshold = .3

NewClass = factor(levels = c("FRAUD","NOTFRAUD"))
NewClass = ifelse(probs>threshold, "FRAUD","NOTFRAUD")
table(NewClass, F[,3])


# Compute a confusion matrix to evaluate the accuracy of predictions; using all available measures
CM_F = confusionMatrix(table(NewClass, F[,3]), mode = "everything")
# Display the confusion matrix
CM_F

#Cost:
Cost_New = CostPerFraud*CM_F$table[2] + CostPerFraudInervention*(CM_F$table[1]+CM_F$table[3])
Cost_New

# balance between cost of the classification and the level of sensitivity
# Do you want to capture all 100% of the frauds?
# Do you want to get most of the frauds and spend less?


# Do a loop over thresholds to optimize
