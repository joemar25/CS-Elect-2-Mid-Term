# Suppress warning messages
options(warn = -1)

# Load required libraries
if (!require(tm)) install.packages("tm")
if (!require(slam)) install.packages("slam")
if (!require(e1071)) install.packages("e1071")

library(tm)
library(tidytext)
library(slam)

library(e1071)

# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]

# Split the data by sentiment
positive_samples <- df[df$Sentiment == "positive", ]
negative_samples <- df[df$Sentiment == "negative", ]

# neutral is not included so numbers might confuse us
n_positive <- nrow(df[df$Sentiment == "positive", ])
n_negative <- nrow(df[df$Sentiment == "negative", ])

# Sample n rows from the positive and negative samples
positive_samples_subset <- positive_samples[sample(nrow(positive_samples), ), ]
negative_samples_subset <- negative_samples[sample(nrow(negative_samples), ), ]

nrow(positive_samples_subset)
nrow(negative_samples_subset)

# Combine the samples into a balanced dataset
unbalanced_df <- rbind(positive_samples_subset, negative_samples_subset)

# Shuffle the rows of the balanced dataset
unbalanced_df <- unbalanced_df[sample(nrow(unbalanced_df)), ]
unbalanced_df$Sentiment <- factor(unbalanced_df$Sentiment, levels = c("positive", "negative"))

# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(unbalanced_df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.99) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- unbalanced_df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Convert dtm_sentiment to a data frame
dtm_sentiment_df <- as.data.frame(dtm_sentiment)

# Convert the dependent variable to a factor
dtm_sentiment_df[, ncol(dtm_sentiment_df)] <- as.factor(dtm_sentiment_df[, ncol(dtm_sentiment_df)])

# Define the number of folds
k <- 5

# Create a train control object for k-fold cross-validation
train_control <- trainControl(method = "cv", number = k)

# Train the Naive Bayes model using k-fold cross-validation
nb_model <- train(x = dtm_sentiment_df[, 1:(ncol(dtm_sentiment_df) - 1)], y = dtm_sentiment_df[, ncol(dtm_sentiment_df)], method = "naive_bayes", trControl = train_control)

# Make predictions on the test set
nb_pred <- predict(nb_model, newdata = dtm_sentiment_df[, 1:(ncol(dtm_sentiment_df) - 1)])

# Get the confusion matrix and calculate performance metrics
conf_mat <- confusionMatrix(data = nb_pred, reference = dtm_sentiment_df[, ncol(dtm_sentiment_df)])
conf_mat

# get the table of confusion matrix
conf_mat_table <- table(nb_pred, dtm_sentiment_df[, ncol(dtm_sentiment_df)])

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