# Library

library(dplyr)
library(class)
library(caret)

# Clear environnement

rm(list = ls())

# Import des données

load(file = "nappe_data_globale_reduite.RData")

# Apprentissage sur les données globales

splitIndex <- createDataPartition(nappe_data_globale_reduite$insertion_barres, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

dependent_variable = nappe_data_globale_reduite$insertion_barres
ordered_labels <- factor(dependent_variable, levels = unique(dependent_variable))


train_nappe_data_globale_reduite <- nappe_data_globale_reduite[splitIndex, ]
test_nappe_data_globale_reduite <- nappe_data_globale_reduite[-splitIndex, ]
train_labels <- ordered_labels[splitIndex]
test_labels <- ordered_labels[-splitIndex]

k <- 5

# Apply KNN to classify test data based on training data (default settings)
predicted_labels <- knn(train = train_nappe_data_globale_reduite[, 6:15,drop=FALSE],
                        test = test_nappe_data_globale_reduite[, 6:15,drop=FALSE], 
                        cl = train_nappe_data_globale_reduite$insertion_barres, k = k)

# Calculate the accuracy of the KNN classifier

accuracy <- sum(predicted_labels == test_nappe_data_globale_reduite$insertion_barres) /
  length(test_nappe_data_globale_reduite$insertion_barres)
cat("Accuracy:", accuracy, "\n")

