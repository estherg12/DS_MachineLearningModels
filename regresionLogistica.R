# Pr(Y=1|X=x)
data("heights")
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# Predicción para un estudiante que mide 66 pulgadas, probabilidad de ser mujer
train_set %>%
  filter(round(height) == 66) %>% # selecciona filas donde la altura redondea a 66
  summarize(y_hat = mean(sex=="Female")) # proporción de mujeres para esa altura (probabilidad condicional)

heights %>% # visualización de probabilidad
  mutate(x = round(height)) %>%
  group_by(x) %>% # agrupa
  filter(n() >= 10) %>% # filtra grupos pequeños
  summarize(prop = mean(sex == "Female")) %>% # proporcion de mujeres
  ggplot(aes(x, prop)) +
  geom_point()

# Convertir factores 0s (male) -> 1s (female), estimando con mínimos cuadrados
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% # valor numérico continuo
  lm(y ~ height, data = .) # ajusta el modelo de regresion lineal simple
# Predicciones
p_hat <- predict(lm_fit, test_set) # probabilidades estimadas con el modelo lineal
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor() # convierte prediciones en clases binarias con umbral de decision 0.5
confusionMatrix(y_hat, test_set$sex)$overall[["Accuracy"]] # exactitud general

# Modelos lineales generalizados
heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  # Las probabilidades no deben ser lineales (modelo pobre)
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2]) # linea recta (modelo lineal)

range(p_hat) # rango

# Modelo logístico
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>% # prepara los datos (female=1, male=0)
  # ajusta el modelo lineal generalizado a regresión logística
  # binomial especifica que el resultado es binario usando función de enlace (logit)
  glm(y ~ height, data = ., family = "binomial")

# Con predict, response asegura que el resultado esté en la escala de probabilidad (0-1), no en escala logit
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response") 
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor # predicción binaria
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]] # exactitud general

# Con más de un predictor (y ~ x_1 + x_2)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2)) # clasificación
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"] # exactitud general

p_hat <- predict(fit_glm, newdata = mnist_27$true_p, type = "response")
mnist_27$true_p %>% mutate(p_hat = p_hat) %>% # visualización de frontera
  ggplot(aes(x_1, x_2, z=p_hat, fill=p_hat)) +
  geom_raster() + # dibuja el mapa de calor
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# EJERCICIOS
# 1. Comparar regresión lineal con regresión logística
make_data <- function(n = 1000, p = 0.5,
                      mu_0 = 0, mu_1 = 2,
                      sigma_0 = 1, sigma_1 = 1){
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  list(train = data.frame(x = x, y = as.factor(y)) %>%
         slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>%
         slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

train_set <- dat$train %>% mutate(y_numeric = as.numeric(y) - 1)
test_set <- dat$test

# Regresion lineal
fit_lm <- lm(y_numeric ~ x, data = train_set)
p_hat_lm <- predict(fit_lm, test_set)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 1, 0), levels = c(0, 1))

# Regresion Logistica
fit_glm <- glm(y ~ x, data = dat$train, family = "binomial") 
p_hat_glm <- predict(fit_glm, test_set, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0), levels = c(0, 1))

# Evaluacion
test_ref_y <- dat$test$y
levels(test_ref_y) <- c(0, 1)
confusionMatrix(data = y_hat_lm, reference = test_ref_y)$overall["Accuracy"]
confusionMatrix(data = y_hat_glm, reference = test_ref_y)$overall["Accuracy"]

# 3. 25 sets (clase 1:Y=0:male:mu0; clase 2:Y=1:female:mu1)
calculate_accuracy <- function(mu_1) {
  # Devuelve un único valor numérico: la exactitud general del modelo logístico para el nivel de separación de clases de mu1
  
  set.seed(42) # Fija la semilla para la generación de datos
  dat <- make_data(mu_1 = mu_1) # genera datos, set de entrenamiento con la separación entre clases
  # Ajuste de Regresión Logística
  fit_glm <- glm(y ~ x, data = dat$train, family = "binomial") 
  # Predicción (binaria) y Evaluación
  p_hat_glm <- predict(fit_glm, dat$test, type = "response")
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0), levels = c(0, 1))
  test_ref_y <- dat$test$y
  levels(test_ref_y) <- c(0, 1) # niveles de predicción
  confusionMatrix(data = y_hat_glm, reference = test_ref_y)$overall["Accuracy"]
}

# delta representa la separación de clases (mu1 - mu0)
delta_values <- seq(0, 3, len = 25) # secuencia de 25 valores

accuracy_results <- map_df(delta_values, function(delta) { # 25 iteraciones
  tibble( # conjunto de datos
    delta = delta,
    accuracy = calculate_accuracy(mu_1 = delta) # separación entre clases
  )
})

# En el punto de partida (delta=0) exactitud cerca de 0.5 xq la diferencia entre medias es casi 0
# Según aumenta delta, las distribuciones se separan
# La clasificación es más fácil cuando las clases están bien separadas
# Cuanto mayor es la separación, mayor es la exactitud general
plot_accuracy <- accuracy_results %>%
  ggplot(aes(x = delta, y = accuracy)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Exactitud vs. Separación de Clases (Delta)",
    x = "Delta (Diferencia de Medias: μ₁ - μ₀)",
    y = "Exactitud (Accuracy) de Regresión Logística"
  )
plot_accuracy
