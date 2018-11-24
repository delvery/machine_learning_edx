#this code loads the basic libraries I am accustomed to using for commands
library(tidyverse)
library(caret)
library(dslabs)

#this code runs the first ML example, using only one predictor
data(heights)
y <- heights$sex
x <- heights$height
#this code creates the training and test sets for the example
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]
#to assess accuracy, create a random sample ignoring predictors
y_hat_1 <- sample(C("Male","Female"), length = test_index, replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat_1 == test_set$sex)
#a second sample predicting male is height is within 2 SD of avg male height
y_hat_2 <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y == y_hat_2)
#now assess different cutoff heights to find the most accurate predictor
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat_3 <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat_3 == train_set$sex)
})
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff