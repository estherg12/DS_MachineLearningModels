library(caret)
library(dplyr)

# 1. Preparación y Partición del Dataset (según lo requerido)
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
# train <- iris[-test_index,] # Solo para definir el cutoff, no es necesario para el cálculo final.

# 2. Definición del Umbral Óptimo (obtenido del análisis del set 'train')
best_cutoff <- 1.7

# 3. Aplicar la Regla de Predicción al conjunto TEST
# Regla: Predecir 'virginica' si Petal.Width > 1.7, 'versicolor' si no.
y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor")
y_hat_factor <- factor(y_hat, levels = levels(test$Species))

# 4. Calcular la Exactitud General
overall_accuracy <- mean(y_hat_factor == test$Species)
overall_accuracy
