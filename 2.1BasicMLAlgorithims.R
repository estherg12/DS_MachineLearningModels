# 1. Dataset type
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Ex 1.1: What proportion of the inclass group is female? 
#What proportion of the online group is female?
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Ex 1.2: Assume that for each class type the students are either 
#all male or all female, based on the most prevalent sex in each class type
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
# Report the accuracy of your prediction of sex based on type
mean(y_hat==y)

# Ex 1.3: sensitivity & specificity
sensitivity(y_hat, y)
specificity(y_hat, y)

# Ex 1.4: prevalence of females
mean(y == "Female")

# 2. Iris Dataset
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Ex 2.1: singular feature in the dataset that yields the greatest
# overall accuracy when predicting species
# predicting virginica if greater than the cutoff and versicolor otherwise
foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
sapply(predictions, max)

# Ex 2.2: use the smart cutoff value from the training data
# to calculate overall accuracy in test data
predictions <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')
mean(y_hat==test$Species)

# Ex 2.3: Which feature produces the second highest accuracy?
foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)

# Now we will perform some exploratory data analysis on the data.
plot(iris, pch=21, bg=iris$Species)
# Ex 2.4: Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train dataset
set.seed(76) 
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1], range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1], range(train$Petal.Width)[2],by=0.1)

# Then, report the overall accuracy when applied to the test dataset
# by creating a rule that predicts virginica if Petal.Length is greater than the length cutoff AND 
# Petal.Width is greater than the width cutoff, and versicolor otherwise
length_predictions <- sapply(petalLengthRange, function(i){
  y_hat <- ifelse(train$Petal.Length>i, 'virginica', 'versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.6

width_predictions <- sapply(petalWidthRange, function(i){
  y_hat <- ifelse(train$Petal.Width>i, 'virginica', 'versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff & test$Petal.Width>width_cutoff, 'virginica', 'versicolor')
# What is the overall accuracy for the test data now?
mean(y_hat==test$Species)