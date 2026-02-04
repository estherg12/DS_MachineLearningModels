library(tidyverse)
library(caret)

# 5.1.1: Variables independientes
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
set.seed(1)

# Cross - Validation
fit <- train(x_subset, y, method = "glm")
fit$results

# 5.1.2: Elegir los mejores predictores
pvals <- rep(0, ncol(x)) # inicializar vector para almacenar p-valores
for (i in 1:ncol(x)) { # iterar sobre cada columna (10.000 de x)
  # realizar la prueba t comparando la media del predictor [i] entre los grupos y=0 e y=1
  # se asumen varianzas iguales
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

# Aplicar el umbral de significancia y contar
ind <- which(pvals < 0.01)
length(ind)

# 5.1.3: Selección sesgada (p-hacking)
set.seed(1)
x_subset <- x[, ind] # usar solo las columnas significantes
fit <- train(x_subset, y, method="glm") # cross - validation GLM (regression)
fit$results$Accuracy

# 5.1.4: kNN
set.seed(1)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
