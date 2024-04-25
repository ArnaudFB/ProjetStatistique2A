# Library

library(dplyr)
library(class)
library(caret)
library(e1071)

# Clear environnement

rm(list = ls())

# Import des données

load(file = "data/nappe_data_config_0_reduite.RData")

cols_to_factor <- c("insertion_barres", "T_entree", "P_rel", "bore", "burn.up")

nappe_data_config_0_reduite <- nappe_data_config_0_reduite %>% 
  mutate_at(vars(cols_to_factor), factor)

summary(nappe_data_config_0_reduite)


############################
dependent_variable = nappe_data_config_0_reduite$bore
ordered_labels <- factor(dependent_variable, levels = unique(dependent_variable))
############################


# Apprentissage sur les données globales

splitIndex <- createDataPartition(dependent_variable, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

train_nappe_data_config_0_reduite <- nappe_data_config_0_reduite[splitIndex, ]
test_nappe_data_config_0_reduite <- nappe_data_config_0_reduite[-splitIndex, ]
train_labels <- ordered_labels[splitIndex]
test_labels <- ordered_labels[-splitIndex]

k <- 5

# Apply KNN to classify test data based on training data (default settings)
predicted_labels <- knn(train = train_nappe_data_config_0_reduite[, -c(1:5),drop=FALSE],
                        test = test_nappe_data_config_0_reduite[, -c(1:5),drop=FALSE], 
                        cl = train_labels, k = k)

# Calculate the accuracy of the KNN classifier

accuracy <- sum(predicted_labels == test_labels) /
  length(test_labels)
cat("Accuracy:", accuracy, "\n")





## Visualisation pour k allant de 1 à 20 de l'accuracy du KNN sur insertion_barre

# Create a vector to store accuracy values for different values of k
accuracy_values <- numeric(length = 10)  # Assuming a maximum k of 20

# Try different values of k and calculate accuracy for each
for (k in 1:10) {
  predicted_labels <- knn(train = train_nappe_data_config_0_reduite[, -c(1:5)], 
                          test = test_nappe_data_config_0_reduite[, -c(1:5)], 
                          cl = train_labels, k = k)
  accuracy_values[k] <- sum(predicted_labels == test_labels) / length(test_labels)
}

# Plot accuracy vs. k
ggplot(data.frame(K = 1:10, Accuracy = accuracy_values), aes(x = K, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Neighbors (k)", y = "Accuracy") +
  ggtitle("Accuracy vs. Number of Neighbors (k)")





## Création de la validation croisée K-fold pour le KNN pour insertion barre

# Create a vector to store accuracy values for different values of k
accuracy_values <- numeric(length = 20)  # Assuming a maximum k of 20

# Perform k-fold cross-validation
k_folds <- 5  # Number of folds
fold_size <- nrow(train_nappe_data_config_0_reduite) %/% k_folds

for (k in 1:20) {
  fold_accuracies <- numeric(k_folds)
  
  for (fold in 1:k_folds) {
    # Split the data into training and validation sets for this fold
    fold_indices <- createDataPartition(train_labels, p = 0.8, list = FALSE)
    validation_data <- train_nappe_data_config_0_reduite[fold_indices, ]
    training_data <- train_nappe_data_config_0_reduite[-fold_indices, ]
    
    # Apply KNN to classify validation data based on training data
    predicted_labels <- knn(train = training_data[, -c(1:5)],
                            test = validation_data[, -c(1:5)], 
                            cl = training_data$bore, k = k)
    
    # Calculate the accuracy for this fold
    fold_accuracies[fold] <- sum(predicted_labels == validation_data$bore) / nrow(validation_data)
  }
  
  # Average accuracy across all folds for this k value
  accuracy_values[k] <- mean(fold_accuracies)
}

# Plot accuracy vs. k
ggplot(data.frame(K = 1:20, Accuracy = accuracy_values), aes(x = K, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Neighbors (k)", y = "Cross-Validation Accuracy") +
  ggtitle("Cross-Validation Accuracy vs. Number of Neighbors (k)")

# Find the optimal k based on the highest cross-validation accuracy
optimal_k <- which.max(accuracy_values)
cat("Optimal k based on cross-validation:", optimal_k, "\n")






## Classifieur bayésien naif

# Create a Naive Bayes classifier
naive_bayes_model <- naiveBayes(bore ~ .,
                                data = train_nappe_data_config_0_reduite)

# Make predictions on the test data
predictions <- predict(naive_bayes_model, test_nappe_data_config_0_reduite)

# Evaluate the model (you can use other evaluation metrics as needed)
confusion_matrix <- table(predictions,
                          test_labels)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
error_rate <- sum(predictions != test_labels) / length(test_labels)

# Print the error rate
cat("Error Rate:", error_rate, "\n")

# Print the confusion matrix and accuracy
cat("Confusion Matrix: Bayésien naïf\n")
print(confusion_matrix)
cat("\nAccuracy:", accuracy, "\n")

