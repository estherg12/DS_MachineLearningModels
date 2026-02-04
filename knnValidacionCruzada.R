library(tidyverse)
library(caret)
library(dslabs)
data("mnist_27")
# Ajustar un modelo kNN
set.seed(1)
train_knn <- train(y ~ ., method = "knn", # train usa validación cruzada
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2))) # probar sistemáticamente todos esos valores

ggplot(train_knn, highlight = TRUE)

# Parámetro que maximiza la exactitud
train_knn$bestTune

# Mejor exactitud q regresión lineal y logística
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), # predict usa el modelo knn para predecir datos nunca antes vistos
                mnist_27$test$y)$overall["Accuracy"] # se comparan las predicciones con las etiquetas reales
# Comparación con Regresión logística = 0.75
# Predicciones
p_hat <- predict(train_knn$finalModel, 
                 newdata = mnist_27$true_p %>% select(x_1, x_2), # <--- Aquí está la corrección
                 type = "prob")
mnist_27$true_p %>% 
  mutate(p_hat = p_hat[, "7"]) %>% # asegurar que solo haya 2 columnas predictoras.
  ggplot(aes(x_1, x_2, z = p_hat, fill = p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors = c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks = c(0.5), color = "black", linewidth = 0.5) +
  labs(title = "Frontera de Decisión kNN Optimizado (k=27)", fill = "P(Y=7)")

# EJERCICIOS
# 1. Sexo basado en la altura con kNN
data("heights")
set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]
k_values <- seq(1, 101, 3) # rango de k a probar
# genera vector con F1Score de 34 modelos de kNN
f1_scores <- sapply(k_values, function(k) {
  # Entrenar kNN (sex ~ height)
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") # Predecir en el test set
  # Calcular F1 score (F_meas), compara predicciones con etiquetas reales
  F_meas(data = y_hat, reference = test_set$sex)
})

max_f1 <- max(f1_scores)
best_k <- k_values[which.max(f1_scores)]

f1_results_df <- data.frame(k = k_values, F1_Score = f1_scores)

f1_results_df %>%
  ggplot(aes(x = k, y = F1_Score)) +
  geom_line(color = "blue") +
  # Destacar el punto con el F1 máximo
  geom_point(aes(x = best_k, y = max_f1), color = "red", size = 3) +
  labs(
    title = "Rendimiento kNN: F1 Score vs. Vecinos (k)",
    subtitle = paste("F1 Máximo:", round(max_f1, 3), "en k =", best_k),
    x = "Número de Vecinos (k)",
    y = "F1 Score"
  )

# 2. tissues
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
# Separar X (predictores) y Y (resultado)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1)
# Creamos índices estratificados
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_x <- x[-test_index, ]
train_y <- y[-test_index]
test_x <- x[test_index, ]
test_y <- y[test_index]
k_values <- seq(1, 11, 2) # k = 1, 3, 5, 7, 9, 11
accuracy_results <- sapply(k_values, function(k) {
  # Entrenar kNN (knn3 toma matriz x y factor y)
  fit <- knn3(train_x, train_y, k = k)
  # Predecir clases en el set de prueba
  y_hat <- predict(fit, test_x, type = "class")
  # Calcular exactitud (proporción de aciertos)
  mean(y_hat == test_y)
})
data.frame(k = k_values, Accuracy = accuracy_results)
