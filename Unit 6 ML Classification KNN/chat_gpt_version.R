#chatgpts version

# Load required libraries
library(class)
library(caret)

# Read Titanic training data from GitHub
titanic <- read.csv(
  "https://raw.githubusercontent.com/BivinSadler/MSDS_6306_Doing-Data-Science/Master/Unit%206/titanic_train.csv"
)

str(titanic)


titanic_sub <- titanic[, c("Survived", "Age", "Pclass", "Sex")]

# Remove rows with missing Age
titanic_sub <- titanic_sub[!is.na(titanic_sub$Age), ]

nrow(titanic_sub)

set.seed(6)

train_index <- sample(1:nrow(titanic_sub), 600)

train <- titanic_sub[train_index, ]
test  <- titanic_sub[-train_index, ]
nrow(train)  # 600
nrow(test)   # ~114 (not 291 anymore due to NA removal)

# Scale predictors using training set parameters
train_x <- scale(train[, c("Age", "Pclass")])
test_x  <- scale(
  test[, c("Age", "Pclass")],
  center = attr(train_x, "scaled:center"),
  scale  = attr(train_x, "scaled:scale")
)

train_y <- train$Survived
test_y  <- test$Survived

set.seed(6)

knn_pred <- knn(
  train = train_x,
  test  = test_x,
  cl    = train_y,
  k     = 5
)

conf_mat <- confusionMatrix(knn_pred, factor(test_y), positive = "1")
conf_mat

accuracy <- conf_mat$overall["Accuracy"]
misclass_rate <- 1 - accuracy
sensitivity <- conf_mat$byClass["Sensitivity"]
specificity <- conf_mat$byClass["Specificity"]

accuracy
misclass_rate
sensitivity
specificity


# How to explain these (plain English)
# 
# Accuracy: overall percent correctly classified
# 
# Misclassification rate: percent classified incorrectly
# 
# Sensitivity: among those who survived, how many we correctly predicted
# 
# Specificity: among those who died, how many we correctly predicted



knn_prob <- knn(
  train = train_x,
  test  = test_x,
  cl    = train_y,
  k     = 5,
  prob  = TRUE
)

prob_survive <- ifelse(
  knn_prob == 1,
  attr(knn_prob, "prob"),
  1 - attr(knn_prob, "prob")
)


new_data <- data.frame(
  Age = rep(75, 3),
  Pclass = c(1, 2, 3)
)

new_scaled <- scale(
  new_data,
  center = attr(train_x, "scaled:center"),
  scale  = attr(train_x, "scaled:scale")
)

knn_new <- knn(
  train = train_x,
  test  = new_scaled,
  cl    = train_y,
  k     = 10,
  prob  = TRUE
)

survival_prob <- ifelse(
  knn_new == 1,
  attr(knn_new, "prob"),
  1 - attr(knn_new, "prob")
)

data.frame(
  Age = 75,
  Pclass = c(1, 2, 3),
  Predicted_Survival = knn_new,
  Survival_Probability = survival_prob
)

train_male   <- train[train$Sex == "male", ]
test_male    <- test[test$Sex == "male", ]

train_female <- train[train$Sex == "female", ]
test_female  <- test[test$Sex == "female", ]

run_knn <- function(train_df, test_df, k = 5) {
  train_x <- scale(train_df[, c("Age", "Pclass")])
  test_x <- scale(
    test_df[, c("Age", "Pclass")],
    center = attr(train_x, "scaled:center"),
    scale  = attr(train_x, "scaled:scale")
  )
  
  pred <- knn(train_x, test_x, train_df$Survived, k = k)
  confusionMatrix(pred, factor(test_df$Survived), positive = "1")
}

male_results   <- run_knn(train_male, test_male)
female_results <- run_knn(train_female, test_female)

male_results
female_results
