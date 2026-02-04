library(tidyverse)
library(dslabs)
mnist <- read_mnist()
# 2 componentes: set de entrenamiento y set de evaluación
names(mnist)

# cada componente -> matriz atributos en columnas
dim(mnist$train$images)
# y vector clases (int)
class(mnist$train$labels)
table(mnist$train$labels)

# ---- 1. Pre-procesamiento ----
# Subconjunto de 10.000 filas aleatorias de train, 1.000 de test
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000) # Elige 10.000 filas al azar
# matriz reducida de predictores
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
index <- sample(nrow(mnist$test$images), 1000) # Elige 1.000 filas al azar
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

# atributos con variabilidad casi 0
library(matrixStats)
sds <- colSds(x) # calcula la sd de cada columna x (píxel) 
qplot(sds, bins = 256)
# identificación
library(caret)
# nearZeroVar analiza cada columna de x y busca columnas con ds casi 0
nzv <- nearZeroVar(x)  # devuelve un vector de números enteros con los índices de columnas a borrar
# columnas
image(matrix(1:784 %in% nzv, 28, 28))
rafalib::mypar()
image(matrix(1:784 %in% nzv, 28, 28))
# borrado
col_index <- setdiff(1:ncol(x), nzv) # guarda todas las columnas EXCEPTO las q están en el índice nzv
length(col_index) # columnas restantes

# Agregar nombres de columna a matrices de predictores
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# ---- 2. kNN ----
control <- trainControl(method = "cv", number = 10, p = .9)
# esto puede llevar días para ejecutarse
train_knn <- train(x[ ,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
train_knn

# subconjunto de datos
n <- 1000
b <- 10
index <- sample(nrow(x), n)
# 90% datos xra entrenar 10% restante
control <- trainControl(method = "cv", number = b, p = .9) # cross-validation en b folds
train_knn <- train(x[index, col_index], y[index], # columnas útiles (x) , etiquetas (y)
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)), # cuadrícula de valores k a probar
                   trControl = control) # cross-validation
best_k <- train_knn$bestTune # valor óptimo de k
fit_knn <- knn3(x[, col_index], y, k = best_k) # entrena modelo con el conjunto completo (x) usando el valor óptimo de k
y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class")

confusionMatrix(y_hat_knn, factor(y_test))$overall["Accuracy"] # exactitud
confusionMatrix(y_hat_knn, factor(y_test))$byClass[, 1:2] # Sensibilidad y especifidad

# ---- 3. Random Forest ----
library(randomForest)
control <- trainControl(method="cv", number = 5) # cross-validation con 5 folds
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100)) # parámetros a probar de mtry
train_rf <- train(x[, col_index], y, # x (matriz predictores con columas útiles), y (etiquetas)
                  method = "rf",
                  ntree = 150, # 150 árboles
                  trControl = control, # cross-validation
                  tuneGrid = grid, # valores mtry a probar
                  nSamp = 5000) # tamaño de muestra bootstrap (aleatoriedad)
fit_rf <- randomForest(x[, col_index], y, # matriz x,y
                       minNode = train_rf$bestTune$mtry)
plot(fit_rf) # gráfico diagnóstico, curva OOB en eje Y vs nº árboles
train_rf$bestTune
y_hat_rf <- predict(fit_rf, x_test[ ,col_index]) # predicciones
cm <- confusionMatrix(y_hat_rf, y_test) 
cm$overall["Accuracy"] #exactitud general

# ---- 4. Importancia Variable ----
imp <- importance(fit_rf) 
mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(mat, 28, 28))

# reajuste
rafalib::mypar()
mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(mat, 28, 28))

# Evaluaciones visuales
p_max <- predict(fit_rf, x_test[,col_index], type = "prob") # probabilidades del random forest
p_max <- p_max / rowSums(p_max) # suma probs x fila (normalización)
p_max <- apply(p_max, 1, max) # probab máx predicha xra cada imagen de prueba
# p_max es un vector de 1.000 elementos, cada uno siendo la confianza del modelo en su predicción

ind  <- which(y_hat_rf != y_test) # índices de imagenes donde el modelo cometió error
ind <- ind[order(p_max[ind], decreasing = TRUE)] # los ordena de mayor a menor

rafalib::mypar(4, 2) # ajusta márgenes para mostrar 1 fila y 4 columnas de gráficos
for(i in ind[1:8]){
  # La función image dibuja el dígito, invirtiendo las columnas [ , 28:1] para la orientación
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        # Título: Predicción (Pr) vs. Clase real
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        # Eliminar ejes
        xaxt="n", yaxt="n")
}

# ---- 5. Conjuntos (ensemble) ----
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
p_rf<- p_rf/ rowSums(p_rf) # suma de probabilidades de cada fila matriz
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2 # media de probs de cada método
y_pred <- factor(apply(p, 1, which.max)-1) # predicción final
confusionMatrix(y_pred, y_test)$overall["Accuracy"] # exacitud de métodos combinados

# ---- EJERCICIOS ----
# Q1: train()
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")
library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2: dimensiones de la matriz de predicciones
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

# Q3: exactitud general del modelo
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

# Q4: voto de mayoría compacto
votes <- rowMeans(pred == "7") # vector de 200 elementos q representa la proporción de modelos q votaron 7 xra cada imagen (media aritmética sobre T/F)
# 200 elementos con la predicción de la clase final
y_hat <- ifelse(votes > 0.5, "7", "2") # si T, y_hat recibe que es un 7, si F, recibe q es un 2
mean(y_hat == mnist_27$test$y) # exactitud general

# Q5: métodos que funcionan mejor q ensemble
ind <- acc > mean(y_hat == mnist_27$test$y) # exactitud general del modelo mayor que la del ensemble
sum(ind) # número de modelos q son mejores q el ensemble
models[ind] # nombre de modelos mejores q ensemble

# Q6: Media de Exactitudes de Entrenamiento (CV)
acc_hat <- sapply(fits, function(fit) 
  min(fit$results$Accuracy)) # minimum accuracy estimates from cross-validation
mean(acc_hat)

# Q7: Estrategia de Ensemble Filtrado por Rendimiento (CV)
ind <- acc_hat >= 0.8 # filtra los modelos por exactitud estimada
votes <- rowMeans(pred[,ind] == "7") # contea solo los votos de los modelos seleccionados
y_hat <- ifelse(votes>=0.5, 7, 2) # predicción final del ensemble
mean(y_hat == mnist_27$test$y)
