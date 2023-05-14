# Cardiño, Joemar BSCS 3C <- Chapter 4


library(tidyverse)
library(caret)

options(warn = -1)
options(digits=3)

data(Zoo, package="mlbench")
Zoo <- as_tibble(Zoo)

# Reserve test data for evaluating the model and use 80% of the data for training
# Create a partition for training and testing data
inTrain <- createDataPartition(y = Zoo$type, p = .8, list = FALSE)
Zoo_train <- Zoo %>% slice(inTrain)
Zoo_test <- Zoo %>% slice(-inTrain)

# Create a fixed sampling scheme with 10 folds for comparing different classification models
# Use the folds in the train() function with the trControl = trainControl() argument
train_index <- createFolds(Zoo_train$type, k = 10)

# Note that datasets with many NA values or those that are highly imbalanced may cause issues with train() and cross-validation.
# To handle such cases, remove columns with many NAs, omit NAs using na.omit(), or use imputation to replace them with reasonable values.
# Highly imbalanced datasets may also result in an error message if a fold does not contain examples of each class
library(party)

ctreeFit <- Zoo_train %>% train(
  type ~ .,
  method = "ctree",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index)
)

ctreeFit
ctreeFit$finalModel
plot(ctreeFit$finalModel)

# Decision Tree
library(RWeka)

# Train a decision tree model using the J48 algorithm
# Use the train() function with the method = "J48" argument
# Tune the model using 5 iterations of parameter tuning with tuneLength = 5
# Use 10-fold cross-validation for model evaluation with trControl = trainControl(method = "cv", indexOut = train_index)
C45Fit <- Zoo_train %>% train(
  type ~ .,
  method = "J48",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index)
)

# Print the trained model object and the final decision tree model
C45Fit
C45Fit$finalModel

# Plot the final decision tree model
plot(C45Fit$finalModel)

# K-Nearest Neighbors
knnFit <- Zoo_train %>% train(
  type ~ .,
  method = "knn",
  data = .,
  preProcess = "scale",
  tuneLength = 5,
  tuneGrid=data.frame(k = 1:10),
  trControl = trainControl(method = "cv", indexOut = train_index)
)

knnFit
knnFit$finalModel

# PART (Rule-based classifier)
rulesFit <- Zoo_train %>% train(
  type ~ .,
  method = "PART",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index)
)

rulesFit
rulesFit$finalModel

# Linear Support Vector Machines
library(kernlab)
svmFit <- Zoo_train %>% train(
  type ~.,
  method = "svmLinear",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index)
)

svmFit
svmFit$finalModel

# Random Forest
library(randomForest)

# Train a random forest model on the Zoo_train dataset using 5-fold cross-validation
# The method argument is set to "rf" to specify that we want to use the random forest algorithm
# The tuneLength argument is set to 5 to specify the number of iterations for tuning the model
# The trControl argument specifies the type of resampling used for cross-validation
randomForestFit <- Zoo_train %>% train(
  type ~ .,
  method = "rf",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index)
)

# Print the results of the random forest model
randomForestFit

# Print the final model object returned by train()
randomForestFit$finalModel

# Gradient Boosted Decision Trees (xgboost)
library(xgboost)
xgboostFit <- Zoo_train %>% train(
  type ~ .,
  method = "xgbTree",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index),
  tuneGrid = expand.grid(
    nrounds = 20,
    max_depth = 3,
    colsample_bytree = .6,
    eta = 0.1,
    gamma=0,
    min_child_weight = 1,
    subsample = .5
  )
)

xgboostFit
xgboostFit$finalModel

# Artificial Neural Network
nnetFit <- Zoo_train %>% train(
  type ~ .,
  method = "nnet",
  data = .,
  tuneLength = 5,
  trControl = trainControl(method = "cv", indexOut = train_index),
  trace = FALSE
)

nnetFit
nnetFit$finalModel

# Comparing Models
resamps <- resamples(
  list(
    ctree = ctreeFit,
    C45 = C45Fit,
    SVM = svmFit,
    KNN = knnFit,
    rules = rulesFit,
    randomForest = randomForestFit,
    xgboost = xgboostFit,
    NeuralNet = nnetFit
  )
)

resamps
summary(resamps)

library(lattice)
bwplot(resamps, layout = c(3, 1))

difs <- diff(resamps)
difs

summary(difs)

# identify the best model for all of the model used
accuracy_means <- sapply(difs$statistics, function(x) x[1])
accuracy_means
best_model <- names(accuracy_means)[which.max(unlist(accuracy_means))]
best_model



# Applying the Chosen Model to the Test Data

# ctreeFit
pr <- predict(ctreeFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# C45Fit
pr <- predict(C45Fit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# svmFit
pr <- predict(svmFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# knnFit
pr <- predict(knnFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# rulesFit
pr <- predict(rulesFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# randomForestFit
pr <- predict(randomForestFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# xgboostFit
pr <- predict(xgboostFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)

# nnetFit
pr <- predict(nnetFit, Zoo_test)
pr
confusionMatrix(pr, reference = Zoo_test$type)


# so far i have learned that there are a lot of alternative ways to train a model.
# Using K-Nearest Neighbors, PART (Rule-based classifier), Linear Support Vector Machines, Random Forest, Gradient Boosted Decision Trees (xgboost), and Artificial Neural Network algorithms.
# Lastly, I have compared those models using resampling and applied the chosen model to test data to evaluate its performance using confusion matrices.





# Deep Learning with keras/tensorflow
if (!require(keras)) install.packages("keras")

library(reticulate)
library(keras)

# First, it we install the keras and tensorflow libraries and activates the conda environment
# where tensorflow is installed. Then, it prepares the data by converting the categorical
# target variable into numerical values using to_categorical() and splitting the dataset
# into training and testing sets.

use_condaenv("r-reticulate")
reticulate::py_install("tensorflow")

X <- Zoo_train %>% select(!type) %>% 
  mutate(across(everything(), as.integer)) %>% as.matrix()
head(X)

y <- Zoo_train %>% pull("type") %>% as.integer() %>% `-`(1L) %>% to_categorical()
head(y)

X <- Zoo_train %>% select(!type) %>% 
  mutate(across(everything(), as.integer)) %>% as.matrix()
head(X)

y <- Zoo_train %>% pull("type") %>% as.integer() %>% `-`(1L) %>% to_categorical()
head(y)

X_test <- Zoo_test %>% select(!type) %>% 
  mutate(across(everything(), as.integer)) %>% as.matrix()
y_test <- Zoo_test %>% pull("type") %>% as.integer() %>% `-`(1L) %>% to_categorical()

model <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(ncol(X)),
              kernel_regularizer=regularizer_l2(l=0.01)) %>%
  layer_dense(units = ncol(y), activation = 'softmax') %>%
  compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')

history <- model %>% fit(
  X, y,
  batch_size = 10,
  epochs = 100,
  validation_split = .2
)

plot(history)

class_labels <- levels(Zoo_train %>% pull(type))

pr <- predict(model, X_test) %>% apply(MARGIN = 1, FUN = which.max)
pr <- factor(pr, labels = class_labels, levels = seq_along(class_labels))

pr
confusionMatrix(pr, reference = Zoo_test$type)



# Comparing Decision Boundaries of Popular Classification Techniques
library(scales)
library(tidyverse)
library(ggplot2)
library(caret)

decisionplot <- function(model, data, class_var, 
                         predict_type = c("class", "prob"), resolution = 5 * 75) {
  # resolution is set to 75 dpi if the image is rendered  5 inces wide. 
  
  y <- data %>% pull(class_var)
  x <- data %>% dplyr::select(-all_of(class_var))
  
  # resubstitution accuracy
  prediction <- predict(model, x, type = predict_type[1])
  # LDA returns a list
  if(is.list(prediction)) prediction <- prediction$class
  prediction <- factor(prediction, levels = levels(y))
  
  cm <- confusionMatrix(data = prediction, reference = y)
  acc <- cm$overall["Accuracy"]
  
  # evaluate model on a grid
  r <- sapply(x[, 1:2], range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each = resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as_tibble(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  cl <- predict(model, g, type = predict_type[1])
  
  # LDA returns a list
  if(is.list(cl)) { 
    prob <- cl$posterior
    cl <- cl$class
  } else
    try(prob <- predict(model, g, type = predict_type[2]))
  
  # we visualize the difference in probability/score between the 
  # winning class and the second best class.
  # don't use probability if predict for the classifier does not support it.
  max_prob <- 1
  try({
    max_prob <- t(apply(prob, MARGIN = 1, sort, decreasing = TRUE))
    max_prob <- max_prob[,1] - max_prob[,2]
  }, silent = TRUE) 
  
  cl <- factor(cl, levels = levels(y))
  
  g <- g %>% add_column(prediction = cl, probability = max_prob)
  
  ggplot(g, mapping = aes_string(
    x = colnames(g)[1],
    y = colnames(g)[2])) +
    geom_raster(mapping = aes(fill = prediction, alpha = probability)) +
    geom_contour(mapping = aes(z = as.numeric(prediction)), 
                 bins = length(levels(cl)), size = .5, color = "black") +
    geom_point(data = data, mapping =  aes_string(
      x = colnames(data)[1],
      y = colnames(data)[2],
      shape = class_var), alpha = .7) + 
    scale_alpha_continuous(range = c(0,1), limits = c(0,1), guide = "none") +  
    labs(subtitle = paste("Training accuracy:", round(acc, 2)))
}










# Iris Dataset
set.seed(1000)
data(iris)
iris <- as_tibble(iris)

### Three classes (MASS also has a select function)
x <- iris %>% dplyr::select(Sepal.Length, Sepal.Width, Species)
x

ggplot(x, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +  
  stat_density_2d(alpha = .2, geom = "polygon") +
  geom_point()



# K-Nearest Neighbors Classifier
library(caret)
model <- x %>% knn3(Species ~ ., data = ., k = 1)
decisionplot(model, x, class_var = "Species") + labs(title = "kNN (1 neighbor)")

model <- x %>% knn3(Species ~ ., data = ., k = 3)
decisionplot(model, x, class_var = "Species") + labs(title = "kNN (3 neighbor)")

model <- x %>% knn3(Species ~ ., data = ., k = 9)
decisionplot(model, x, class_var = "Species") + labs(title = "kNN (9 neighbor)")

# Naive Bayes Classifier
library(e1071)
model <- x %>% naiveBayes(Species ~ ., data = .)
decisionplot(model, x, class_var = "Species", predict_type = c("class", "raw")) + labs(title = "Naive Bayes")

# Linear Discriminant Analysis
library(MASS)
model <- x %>% lda(Species ~ ., data = .)
decisionplot(model, x, class_var = "Species") + labs(title = "LDA")

# Multinomial Logistic Regression (implemented in nnet)
library(nnet)
model <- x %>% multinom(Species ~., data = .)
decisionplot(model, x, class_var = "Species") + labs(titel = "Multinomial Logistic Regression")

# Decision Trees
library("rpart")
model <- x %>% rpart(Species ~ ., data = .)
decisionplot(model, x, class_var = "Species") + labs(title = "CART")

model <- x %>% rpart(Species ~ ., data = .,
                     control = rpart.control(cp = 0.001, minsplit = 1))
decisionplot(model, x, class_var = "Species") + labs(title = "CART (overfitting)")

if (!require(C50)) install.packages("C50")
library(C50)
model <- x %>% C5.0(Species ~ ., data = .)
decisionplot(model, x, class_var = "Species") + labs(title = "C5.0")

library(randomForest)
model <- x %>% randomForest(Species ~ ., data = .)
decisionplot(model, x, class_var = "Species") + labs(title = "Random Forest")

# SVM
library(e1071)
model <- x %>% svm(Species ~ ., data = ., kernel = "linear")
decisionplot(model, x, class_var = "Species") + labs(title = "SVM (linear kernel)")

model <- x %>% svm(Species ~ ., data = ., kernel = "radial")
decisionplot(model, x, class_var = "Species") + labs(title = "SVM (radial kernel)")

model <- x %>% svm(Species ~ ., data = ., kernel = "polynomial")
decisionplot(model, x, class_var = "Species") + labs(title = "SVM (polynomial kernel)")

model <- x %>% svm(Species ~ ., data = ., kernel = "sigmoid")
decisionplot(model, x, class_var = "Species") + labs(title = "SVM (sigmoid kernel)")

# Single Layer Feed-forward Neural Networks
library(nnet)
model <-x %>% nnet(Species ~ ., data = ., size = 1, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var  = "Species", 
             predict_type = c("class", "raw")) + labs(title = "NN (1 neuron)")

model <-x %>% nnet(Species ~ ., data = ., size = 2, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var  = "Species", 
             predict_type = c("class", "raw")) + labs(title = "NN (2 neurons)")

model <-x %>% nnet(Species ~ ., data = ., size = 4, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var  = "Species", 
             predict_type = c("class", "raw")) + labs(title = "NN (4 neurons)")

model <-x %>% nnet(Species ~ ., data = ., size = 10, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var  = "Species", 
             predict_type = c("class", "raw")) + labs(title = "NN (10 neurons)")



# I learned about k-Nearest Neighbors, Naive Bayes, Linear Discriminant Analysis,
# Multinomial Logistic Regression, Decision Trees, Support Vector Machines, and
# Single Layer Feed-forward Neural Networks and how to implement and visualize
# them in R using the new code.









# Circle Dataset
set.seed(1000)

library(mlbench)
x <- mlbench.circle(500)
x <- cbind(as.data.frame(x$x), factor(x$classes))
colnames(x) <- c("x", "y", "class")
x <- as_tibble(x)
x

ggplot(x, aes(x = x, y = y, color = class)) + 
  geom_point()

# K-Nearest Neighbors Classifier
library(caret)
model <- x %>% knn3(class ~ ., data = ., k = 1)
decisionplot(model, x, class_var = "class") + labs(title = "kNN (1 neighbor)")

model <- x %>% knn3(class ~ ., data = ., k = 10)
decisionplot(model, x, class_var = "class") + labs(title = "kNN (10 neighbor)")

# Naive Bayes Classifier
library(e1071)
model <- x %>% naiveBayes(class ~ ., data = .)
decisionplot(model, x, class_var = "class", 
             predict_type = c("class", "raw")) + labs(title = "naive Bayes")

# Linear Discriminant Analysis
library(MASS)
model <- x %>% lda(class ~ ., data = .)
decisionplot(model, x, class_var = "class") + labs(title = "LDA")

# Multinomial Logistic Regression (implemented in nnet)
library(nnet)
model <- x %>% multinom(class ~., data = .)
decisionplot(model, x, class_var = "class") + labs(titel = "Multinomial Logistic Regression")

# Decision Trees
library("rpart")
model <- x %>% rpart(class ~ ., data = .)
decisionplot(model, x, class_var = "class") + labs(title = "CART")

model <- x %>% rpart(class ~ ., data = .,
                     control = rpart.control(cp = 0.001, minsplit = 1))
decisionplot(model, x, class_var = "class") + labs(title = "CART (overfitting)")

library(C50)
model <- x %>% C5.0(class ~ ., data = .)
decisionplot(model, x, class_var = "class") + labs(title = "C5.0")

library(randomForest)
model <- x %>% randomForest(class ~ ., data = .)
decisionplot(model, x, class_var = "class") + labs(title = "Random Forest")


# The code above implements k-Nearest Neighbors, Naive Bayes, Linear Discriminant Analysis,
# Multinomial Logistic Regression, Decision Trees, C5.0, and Random Forest on the
# Circle Dataset using the mlbench and caret libraries in R.





# SVM
library(e1071)
model <- x %>% svm(class ~ ., data = ., kernel = "linear")
decisionplot(model, x, class_var = "class") + labs(title = "SVM (linear kernel)")

model <- x %>% svm(class ~ ., data = ., kernel = "radial")
decisionplot(model, x, class_var = "class") + labs(title = "SVM (radial kernel)")

model <- x %>% svm(class ~ ., data = ., kernel = "polynomial")
decisionplot(model, x, class_var = "class") + labs(title = "SVM (polynomial kernel)")

model <- x %>% svm(class ~ ., data = ., kernel = "sigmoid")
decisionplot(model, x, class_var = "class") + labs(title = "SVM (sigmoid kernel)")

# Single Layer Feed-forward Neural Networks
library(nnet)
model <-x %>% nnet(class ~ ., data = ., size = 1, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var = "class", 
             predict_type = c("class", "raw")) + labs(title = "NN (1 neuron)")

model <-x %>% nnet(class ~ ., data = ., size = 2, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var = "class", 
             predict_type = c("class", "raw")) + labs(title = "NN (2 neurons)")

model <-x %>% nnet(class ~ ., data = ., size = 4, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var = "class", 
             predict_type = c("class", "raw")) + labs(title = "NN (4 neurons)")

model <-x %>% nnet(class ~ ., data = ., size = 10, maxit = 1000, trace = FALSE)
decisionplot(model, x, class_var = "class", 
             predict_type = c("class", "raw")) + labs(title = "NN (10 neurons)")

# I learned that the library "e1071" can be used to create support vector machines
# (SVM) with different types of kernel functions including linear, radial, polynomial,
# and sigmoid. SVMs are a type of machine learning algorithm that can be used for
# classification and regression analysis.

# I also learned that the "nnet" library can be used to create single-layer
# feed-forward neural networks. The number of neurons in the network can be specified
# by the "size" parameter. By adjusting the number of neurons in the network,
# we can explore how the model's performance changes with respect to its complexity.







# Overall, we test classification machine learning algorithms.
# Where we used k-nearest neighbors, naive Bayes, linear discriminant analysis, 
# multinomial logistic regression, decision trees, random forest, support vector
# machines, and single-layer feed-forward neural networks.

# We also tried different hyperparameters for each algorithm, such as the number
# of neighbors for kNN, the kernel for SVM, and the number of neurons for neural
# networks.

# And I have seen the comparison of each machine learning algorithms on datasets
# to find the best one for the job.





