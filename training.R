# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)

summary(df)

data <- df[, c("Summary", "Sentiment")]














if(!require(caTools)) install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(data$Sentiment, SplitRatio = 0.8)
train_data <- data[split, ]
test_data <- data[!split, ]

if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

if(!require(caret)) install.packages("caret")
library(caret)

# Train the model
model <- randomForest(as.factor(Sentiment) ~ ., data = train_data)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Convert predictions to a factor with the same levels as test_data$Sentiment
predictions <- factor(predictions, levels = levels(test_data$Sentiment))

# Evaluate the performance of the model
confusionMatrix(predictions, test_data$Sentiment)




# depende sa algo kung gano kabilis ma train ung set natin






