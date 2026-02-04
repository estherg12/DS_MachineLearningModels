# PRIMERA PARTE
# Usar rpart para ajustar un árbol de regresión
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
plot(fit)
text(fit)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

# random forest
library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)

# nodesize = 50 y max 25 nodes
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
# unsmooth  
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# SEGUNDA PARTE
# cp con mayor exactitud
library(caret)
library(rpart)
library(dslabs)
library(tidyverse)

data("tissue_gene_expression")
set.seed(1991)
grid <- data.frame(cp = seq(0, 0.1, 0.01))
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
fit$results
ggplot(fit)
fit$bestTune

# control = rpart.control(minsplit = 0)
set.seed(1991)
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
max(fit_rpart$results$Accuracy)
confusionMatrix(fit_rpart)
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# nodesize = 1
set.seed(1991)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)
fit$results
