# BREAST CANCER - brca: diagnosis biopsy
# brca$y: clasifications (B/M: benign / malignant)
# brca$x: matrix properties shape+size cell

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# ---- Q1: dimesiones y propiedades ----
nrow(brca$x) # nº de muestras: filas de la matriz de predictores
ncol(brca$x) # predictores en la matriz (características): columnas en x
mean(brca$y == "M") # propoción de muestras malignas

which.max(colMeans(brca$x)) # columna con el valor máximo mean

which.min(colSds(brca$x)) # columna con valor mínimo sd

# ---- Q2: estandarización la matriz ----
col_means <- colMeans(brca$x) # medias x columnas
col_sds <- matrixStats::colSds(brca$x) # sd x columnas
x_centered <- sweep(brca$x, 2, col_means, FUN = "-") # restar la media de cada columna
x_scaled <- sweep(x_centered, 2, col_sds, FUN = "/") # dividir por la desviación estándar

sd(x_scaled[,1]) # desviación estándar de la 1ª columna escalada
median(x_scaled[,1]) # mediana de la 1ª columna escalada

# ---- Q3: PCA ----
pca <- prcomp(x_scaled) # PCA sobre la matriz escalada
summary(pca)    # see PC1 Cumulative Proportion
# first value of Cumulative Proportion that exceeds 0.9: PC7
pve_acumulada <- cumsum(pve) # suma acumulada de proporciones
min(which(pve_acumulada >= 0.90)) # compomentes necesarios para explicar al menos 90% de varianza

# ---- Q4: plotting PCs ----
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()
# Malignant tumors tend to have larger values of PC1 than benign tumors.

# ---- Q5: boxplot first 10 PCs grouped by tumor type ----
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()
# When you look at the boxplot, you can see that the IQRs overlap for PCs 2 through 10 but not for PC1.

# TRAIN AND TEST SETS
set.seed(1) 
# Partición
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

# ---- Q6: training and test sets ----
mean(train_y == "B") # proporción B en train
mean(test_y == "B") # proporción B en test

# ---- Q7: Modelo de Regresión Lógica (glm) ----
set.seed(1)
predict_glm <- train(train_x, # train_x como predictores 
                     train_y, # train_y como variable objetivo
                     method = "glm") # especifica regresión logística
y_hat_glm <- predict(predict_glm, test_x) # predicciones sobre el conjunto de prueba con el modelo entrenado
accuracy_glm <- mean(y_hat_glm == test_y) # precisión (comparación de predicciones con valores reales)
accuracy_glm

# ---- Q8: Modelo Loess ----
library(gam)
set.seed(5)
# La siguiente línea de código puede llevar unos minutos
fit_loess <- train(train_x, train_y, 
                   method = "gamLoess") # entrenar el modelo con loess
y_hat_loess <- predict(fit_loess, test_x) # predicciones sobre el conjunto de prueba
accuracy_loess <- mean(y_hat_loess == test_y) # precisión (accuracy)
accuracy_loess

# ---- Q9: Knn Model ----
set.seed(7)
tuning <- data.frame(k=seq(3, 21, 2)) # nºs impares del 3 al 21
fit_knn <- train(train_x, # matriz de predictores de entrenamiento
                 train_y, # etiquetas de entrenamiento
                 method = "knn", # algoritmo
                 tuneGrid = tuning) # lista de valores k a probar

fit_knn$bestTune$k # mejor valor k, que maximiza Accuracy en train
y_hat_knn <- predict(fit_knn, test_x) # predicciones sobre el conjunto de prueba usando el modelo final
accuracy_knn <- mean(y_hat_knn == test_y) # precisión (accuracy)
accuracy_knn

# ---- Q10: random forest model ----
set.seed(9)
tuning <- data.frame(mtry = c(3, 5, 7, 9)) # predictores seleccionados al azar en cada división del árbol
fit_rf <- train(train_x, train_y, 
                method = "rf", 
                tuneGrid = tuning,
                importance = TRUE) # analizar qué variables son más críticas después
fit_rf$bestTune$mtry # mejor valor de mtry
y_hat_rf <- predict(fit_rf, test_x) # predicciones sobre el conjunto de prueba
accuracy_rf <- mean(y_hat_rf == test_y) # precisión (accuracy)
accuracy_rf
varImp(fit_rf) # importancia de variables basada en el modelo entrenado
# worst values representan las variables con más importancia para determinar el tipo de tumor

# ---- Q11: ensemble, votación mayoritaria por conjunto ----
ensemble <- cbind(glm = y_hat_glm == "B", 
                  loess = y_hat_loess == "B", 
                  rf = y_hat_knn == "B", 
                  knn = y_hat_rf == "B")
# Calcular la predicción por mayoría: xra cada fila contamos cuántos modelos predijeron M
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M") # Si la cuenta es >= 3, predecimos M
accuracy_ensemble <- mean(ensemble_preds == test_y) # precisión (accuracy) del Ensemble
accuracy_ensemble

# Tabla con los valores de Accuracy
models <- c("Logistic regression", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(y_hat_glm == test_y),
              mean(y_hat_loess == test_y),
              mean(y_hat_knn == test_y),
              mean(y_hat_rf == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)
