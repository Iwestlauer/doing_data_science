# Load required libraries for data manipulation, machine learning, and plotting
library(caret)
library(tidyverse)
library(class)
library(pROC)

#Cost Variables
CostPerFraud = 10000
CostPerFraudInervention = 1000

# Read data from a user-specified CSV file, expecting headers and converting strings to factors
F = read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE)
# Display the first few rows of the dataset to verify its structure
head(F)

#BASELINE KNN CLASSFICATION 
#Unbalanced data with threshhold = .5
# Perform K-nearest neighbors classification; using columns 1 and 2 as predictors and column 3 as the target
classifications = knn(F[,1:2], F[1:2], F[,3], prob = TRUE, k = 11)
# Create a table of predicted vs actual values for assessing classification accuracy
table(classifications, F[,3])

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
classifications[1:200] # Just the lables
probs

# Calculate and display the proportion of "FRAUD" cases in the dataset
summary(F$Label)
threshold = .3 # Define a new threshold to identify "FRAUD" more liberally

# Apply the new threshold to reclassify observations
NewClass = ifelse(probs > threshold, "FRAUD", "NOTFRAUD")
# Tabulate the new classifications against actual values
table(NewClass, F[,3])

# Compute a confusion matrix to evaluate the accuracy of predictions; using all available measures
CM_F = confusionMatrix(table(NewClass, F[,3]), mode = "everything")
# Display the confusion matrix
CM_F

#Cost:
Cost_New_Thresh = CostPerFraud*CM_F$table[2] + CostPerFraudInervention*(CM_F$table[1]+CM_F$table[3])
Cost_New_Thresh



# Naive Bayes
library(e1071)
nb_model = naiveBayes(F[1:2],F[3])
classificationsNB = predict(nb_model,F[1:2], type = "class")
classificationsNB

# Compute a confusion matrix to evaluate the accuracy of predictions; using all available measures
CM_F = confusionMatrix(table(classificationsNB, F[,3]), mode = "everything")
# Display the confusion matrix
CM_F

#Cost:
Cost_Base = CostPerFraud*CM_F$table[2] + CostPerFraudInervention*(CM_F$table[1]+CM_F$table[3])
Cost_Base
