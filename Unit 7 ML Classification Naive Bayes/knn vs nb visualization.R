knn_accuracies = c(0.6963, 0.6449, 0.6308, 0.6542, 0.6776)
nb_accuracies  = c(0.729, 0.7196, 0.6963, 0.6402, 0.6729)
seeds = c(4,8,16,32,64)

x_vals = 1:length(seeds)

# First plot (kNN)
plot(x_vals, knn_accuracies,
     type = "b",
     col = "blue",
     pch = 19,
     xaxt = "n",
     ylim = range(c(knn_accuracies, nb_accuracies)),
     xlab = "Seed",
     ylab = "Accuracy",
     main = "kNN vs Naive Bayes Accuracy")

# Add custom x-axis labels
axis(1, at = x_vals, labels = seeds)

# Add NB line
lines(x_vals, nb_accuracies,
      type = "b",
      col = "red",
      pch = 19)

# Add legend
legend("topright",
       legend = c("kNN", "Naive Bayes"),
       col = c("blue", "red"),
       pch = 19,
       lty = 1)
