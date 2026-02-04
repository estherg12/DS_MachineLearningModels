library(titanic)
library(caret)
library(tidyverse)
library(rpart)

# Configuración de dígitos significativos
options(digits = 3)

# Limpia de datos: imputar edad, crear factores y nuevas variables
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# ---- Cuestión 1: Training and test sets ---- 
set.seed(42)

# Partición del 20% para test set columna Survived
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
# Asignar 20% a test_set y 80% a train_set
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

nrow(train_set) # nº observaciones train
nrow(test_set) # nº observaciones test
mean(train_set$Survived == 1) # proporción individual train

# ---- Cuestión 2: Baseline prediction by guessing the outcome ---- 
set.seed(3)
# sample a c(0,1) tantas veces como filas haya en el test
# replace=TRUE para sacar +2 muestras de un vector de 2 opciones
y_hat_guessing <- sample(c(0, 1), nrow(test_set), replace = TRUE)
mean(y_hat_guessing == as.character(test_set$Survived)) # exactitud

# ---- Cuestión 3a: Predicting survival by sex ---- 
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

# ---- Cuestión 3b: Predicting survival by sex ---- 
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy (exactitud general)

# ---- Cuestión 4a: Predicting survival by passenger class ----
# Clases con mayor probabilidad de sobrevivir (> 50%)
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

# ---- Cuestión 4b: Predicting survival by passenger class ----
class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy (exactitud general)

# ---- Cuestión 4c: Predicting survival by passenger class ----
train_set %>%
  group_by(Sex, Pclass) %>% # sexo y clase
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5) # mayor probabilidad de supervivencia (>50%)

# ---- Cuestión 4d: Predicting survival by passenger class ----
# Supervivencia si el ratio para sexo/clase es > 0.5
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)

# ---- Cuestión 5a: Confusion Matrix ----
y_hat_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"), levels = levels(test_set$Survived))
y_hat_class <- factor(ifelse(test_set$Pclass == 1, "1", "0"), levels = levels(test_set$Survived))

survival_rates <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Rate = mean(Survived == "1"), .groups = "drop")

y_hat_sex_class <- test_set %>%
  left_join(survival_rates, by = c("Sex", "Pclass")) %>%
  mutate(pred = factor(ifelse(Rate > 0.5, "1", "0"), levels = levels(test_set$Survived))) %>%
  pull(pred)

cm_sex <- confusionMatrix(data = y_hat_sex, reference = test_set$Survived)
cm_class <- confusionMatrix(data = y_hat_class, reference = test_set$Survived)
cm_combo <- confusionMatrix(data = y_hat_sex_class, reference = test_set$Survived)

metrics_sex <- cm_sex$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")]
metrics_class <- cm_class$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")]
metrics_combo <- cm_combo$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")]

data.frame(
  Model = c("Sex Only", "Class Only", "Sex + Class"),
  Sensitivity = c(metrics_sex["Sensitivity"], metrics_class["Sensitivity"], metrics_combo["Sensitivity"]),
  Specificity = c(metrics_sex["Specificity"], metrics_class["Specificity"], metrics_combo["Specificity"]),
  Balanced_Accuracy = c(metrics_sex["Balanced Accuracy"], metrics_class["Balanced Accuracy"], metrics_combo["Balanced Accuracy"])
)

# ---- Cuestión 6: F1 scores ----
F_meas(data = factor(sex_model), reference = test_set$Survived)
F_meas(data = factor(class_model), reference = test_set$Survived)
F_meas(data = factor(sex_class_model), reference = test_set$Survived)

# ---- Cuestión 7: Survival by fare - Loess ----
set.seed(1) 
train_loess <- train(Survived ~ Fare, method = "gamLoess", data = train_set)
loess_preds <- predict(train_loess, test_set)
mean(loess_preds == test_set$Survived) # exactitud general

# ---- Cuestión 8: Logistic Regression Models
set.seed(1) 
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)

set.seed(1) 
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

set.seed(1)
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

# ---- Cuestión 9a: kNN Model ----
set.seed(6)
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

# ---- Cuestión 9b: kNN Model ----
ggplot(train_knn, highlight = TRUE)
plot(train_knn)

# ---- Cuestión 9c: kNN Model ----
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

# ---- Cuestión 10: Cross - Validation ----
set.seed(8)
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune # Valor óptimo de k

knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived) # exactitud

# ---- Cuestión 11a: Classification Tree Model ----
set.seed(10)
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune # Valor óptimo de cp
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived) # exactitud

# ---- Cuestión 11b: Classification Tree Model ----
train_rpart$finalModel # inspect final model
# make plot of decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)

# ---- Cuestión 11c: Classification Tree Model ----
individuals <- data.frame(
  Sex = c("male", "female", "female", "male", "female", "female", "male"),
  Age = c(28, NA, NA, 5, NA, 17, 17), # Ponemos NA donde la edad no se especifica (usaremos la mediana como en el training)
  Pclass = c(1, 2, 3, 3, 3, 1, 1),    # Asumimos clases genéricas donde no se especifica, pero ajustamos a la descripción
  Fare = c(50, 50, 8, 10, 25, 100, 100), # Asumimos tarifas típicas si no se dicen
  SibSp = c(0, 0, 0, 4, 0, 2, 2),
  Parch = c(0, 0, 0, 0, 0, 0, 0),
  Embarked = factor(c("S", "S", "S", "S", "S", "S", "S"), levels = levels(train_set$Embarked))
)
individuals$FamilySize <- individuals$SibSp + individuals$Parch + 1
individuals$Age[is.na(individuals$Age)] <- 28
predictions <- predict(train_rpart, individuals, type = "raw")
data.frame(
  Caso = c("a) Male, 28", 
           "b) Female, Pclass 2", 
           "c) Female, Pclass 3, Fare $8", 
           "d) Male, 5, 4 siblings", 
           "e) Female, Pclass 3, Fare $25", 
           "f) Female, Pclass 1, 17", 
           "g) Male, Pclass 1, 17"),
  Prediccion = ifelse(predictions == 1, "Sobrevive", "Muere")
)

# ---- Cuestión 12: Random Forest Model ----
set.seed(14)
train_rf <- train(Survived ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune # Valor óptimo de mtry
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived) # exactitud