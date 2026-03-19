library(caret)
library(class)
data(iris)

irisVersVirg = iris %>% filter(Species == "versicolor" | Species == "virginica")
irisVersVirg = droplevels(irisVersVirg,exclude = "setosa")

#knn.cv always performs leave one out cross validation (LOOCV) the cv is for cross validation

x <- irisVersVirg[, 1:4]      # Sepal & Petal measurements ( the explanatory variables )
y <- irisVersVirg$Species    # Class labels ( the response variable )

set.seed(123)

pred_cv <- knn.cv(train = x, cl = y, k = 10)
confusion_matrix <- table(True = y, Predicted = pred_cv)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy #0.95

true_positive <- confusion_matrix["virginica", "virginica"]
false_negative <- confusion_matrix["virginica", "versicolor"]
false_positive <- confusion_matrix["versicolor", "virginica"]
true_negative <- confusion_matrix["versicolor", "versicolor"]

sensitivity <- true_positive / (true_positive + false_negative)
sensitivity #0.96, same answer whether or not I drop setosa??

specificity <- true_negative / (true_negative + false_positive)
specificity





# A k-nearest neighbors classifier with k = 10 was evaluated using 100-fold cross-validation 
# on the Iris dataset restricted to Versicolor and Virginica species. 
# Because the dataset contains 100 observations, this procedure is equivalent to 
# leave-one-out cross-validation, which was implemented using the knn.cv() function. 
# Classification accuracy was computed by comparing predicted and true class labels.

