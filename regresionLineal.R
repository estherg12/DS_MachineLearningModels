library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

library(HistData)
set.seed(1983)
# Cadena de manipulación de datos
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>% # filtra las observaciones para quedarse solo con los varones
  group_by(family) %>% # agrupa los datos por la variable familia
  sample_n(1) %>% # selecciona aleatoriamente una única fila por familia (1 solo hijo)
  ungroup() %>% # elimina la agrupación para que las siguientes operaciones se apliquen a todo el dataframe
  select(father, childHeight) %>% # selecciona solo las columnas de interés (father y childHeight)
  rename(son = childHeight) # cambia el nombre de la columna

# Predecir la altura del hijo Y usando la altura del padre X
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index) # crea conjunto de entrenamiento
test_set <- galton_heights %>% slice(test_index)
m <- mean(train_set$son) # media de altura de hijos de train
sqrt(mean((m - test_set$son)^2)) # pérdida cuadrática (RMSE)

# Ajusta el modelo Lineal (lm) 
fit <- lm(son ~ father, data = train_set)
fit$coef # muestra los coeficientes estimados por el modelo 
#interceptación: altura promedio cuando el padre mide 0
#pendiente: cambio en altura del hijo por cada pulgada de cambio en la altura del padre
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
sqrt(mean((y_hat - test_set$son)^2)) # RMSE

# Con predict
y_hat <- predict(fit, test_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$son)^2)) # RMSE
