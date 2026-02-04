# 4.1: Nearest Neighbors
# Ex 4.1.1: Predicción de Sexo basado en Altura con kNN
library(dslabs)
library(caret)
library(dplyr)
data("heights")
set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]
k_values <- seq(1, 101, 3)
# Calcular F1 scores usando sapply
f1_results <- sapply(k_values, function(k){
  # Entrenar el modelo kNN con el set de entrenamiento
  fit <- knn3(sex ~ height, data = train_set, k = k)
  # Predecir en el set de prueba
  # type = "class" nos devuelve la clase (Male/Female) en lugar de probabilidades
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  # Calcular F_meas (F1 score)
  # Por defecto, F_meas usa la primera clase alfabética ("Female") como referencia positiva
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(k_values, f1_results)
max(f1_results)
k_values[which.max(f1_results)]

# Ex 4.1.2: Predicción de Tejidos con Expresión Génica
library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
# createDataPartition se usa fuera del sapply como indica el enunciado
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

# 4.2: Bootstrap
# Ex 4.2.1: Conteo en el Primer Indice Remuestreado
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10) # genera 10 sets de índices de remuestreo
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

# Ex 4.2.2: Conteo Total en Todos los índices
x=sapply(indexes, function(ind){ # sapply itera sobre la lista de índices
  sum(ind == 3) # para cada índice cuenta las veces q aparece 3
})
sum(x) # suma los conteos de las 10 repeticiones

# Ex 4.2.3: Simulación Monte Carlo para 75º Cantil (Q3)
set.seed(1)
B <- 10000
quantiles_mc <- replicate(B, {
  # generar nuevo dataset de 100 observaciones de N(0, 1)
  y_sample <- rnorm(100, 0, 1) # rnorm genera números aleatorios de una distribución normal
  quantile(y_sample, 0.75)
})
mean(quantiles_mc)
sd(quantiles_mc)

# Ex 4.2.4: Simulación Bootstrap para 75º Cuantil (Q3)
set.seed(1)
y <- rnorm(100, 0, 1)

set.seed(1)
indexes <- createResample(y, 10000) # genera una lista de 10.000 vectores de 100 índices
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind] # crea la muestra aplicando los índices al vector original
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
