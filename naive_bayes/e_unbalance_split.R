# Suppress warning messages
options(warn = -1)

# Load required libraries
if (!require(tm)) install.packages("tm")
if (!require(e1071)) install.packages("e1071")
if (!require(caret)) install.packages("caret")
if (!require(tidytext)) install.packages("tidytext")

library(tm)
library(e1071)
library(caret)
library(tidytext)

# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]

# Split the data by sentiment
positive_samples <- df[df$Sentiment == "positive", ]
negative_samples <- df[df$Sentiment == "negative", ]

# Sample n rows from the positive and negative samples
positive_samples_subset <- positive_samples[sample(nrow(positive_samples), ), ]
negative_samples_subset <- negative_samples[sample(nrow(negative_samples), ), ]

# Combine the samples into a balanced dataset
unbalanced_df <- rbind(positive_samples_subset, negative_samples_subset)

# Shuffle the rows of the balanced dataset
set.seed(123)
unbalanced_df <- unbalanced_df[sample(nrow(unbalanced_df)), ]

# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(unbalanced_df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.99) # Remove sparse terms
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- unbalanced_df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Convert dtm_sentiment to a data frame
dtm_sentiment_df <- as.data.frame(dtm_sentiment)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(dtm_sentiment_df), nrow(dtm_sentiment_df) * 0.8)
train_data <- dtm_sentiment_df[train_indices, ]
test_data <- dtm_sentiment_df[-train_indices, ]

# Convert the dependent variable to a factor
train_data[, ncol(train_data)] <- as.factor(train_data[, ncol(train_data)])
test_data[, ncol(test_data)] <- as.factor(test_data[, ncol(test_data)])

# Train the Naive Bayes model
nb_model <- naiveBayes(x = train_data[, 1:(ncol(train_data) - 1)], y = train_data[, ncol(train_data)])

# Make predictions on test data
nb_pred <- predict(nb_model, newdata = test_data[, 1:(ncol(test_data) - 1)])

# Get the confusion matrix and calculate performance metrics
conf_mat <- confusionMatrix(data = nb_pred, reference = test_data[, ncol(test_data)])
conf_mat

# Get the confusion matrix and calculate performance metrics
conf_mat_table <- confusionMatrix(data = nb_pred, reference = test_data[, ncol(test_data)])

# Print the confusion matrix (both alternatives)
conf_mat_table
conf_mat$table

# Print performance metrics
cat("Accuracy:", round(conf_mat$overall["Accuracy"] * 100, 2), "\n")
cat("Precision:", round(conf_mat$byClass["Precision"], 2), "\n")
cat("Recall:", round(conf_mat$byClass["Recall"], 2), "\n")
cat("F1-score:", round(conf_mat$byClass["F1"], 2), "\n")

# Testing -> Define the text labels
sentiment_labels <- c("positive", "negative")

# Preprocess new summary
new_summary <- "this is a bad product"
new_corpus <- Corpus(VectorSource(new_summary))
new_dtm <- DocumentTermMatrix(new_corpus, control = list(stopwords = TRUE, minDocFreq = 10))
new_dtm <- removeSparseTerms(new_dtm, 0.99)
new_dtm <- as.matrix(new_dtm)

# Predict sentiment of new summary using trained model
new_pred <- predict(nb_model, newdata = new_dtm)

# Print predicted sentiment label
cat("Predicted sentiment label for new summary:", sentiment_labels[new_pred], "\n")