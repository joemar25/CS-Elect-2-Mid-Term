# Suppress warning messages
options(warn = -1)

# Load required libraries
if (!require(tm)) install.packages("tm")
if (!require(rpart)) install.packages("rpart")
if (!require(rpart)) install.packages("rpart.plot")
if (!require(caret)) install.packages("caret")
if (!require(tidytext)) install.packages("tidytext")

library(tm)
library(rpart)
library(rpart.plot)
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

# Convert Sentiment to a factor with levels "positive" and "negative"
unbalanced_df$Sentiment <- factor(unbalanced_df$Sentiment, levels = c("positive", "negative"))

# Shuffle the rows of the balanced dataset
set.seed(123)
unbalanced_df <- unbalanced_df[sample(nrow(unbalanced_df)), ]

# Create a corpus of the text summaries
corpus <- VCorpus(VectorSource(unbalanced_df$Summary))

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

# Train the decision tree model
last_col_name <- names(train_data)[ncol(train_data)]
dt_formula <- as.formula(paste(last_col_name, "~ ."))
dt_model <- rpart(dt_formula, data = train_data, method = "class")

importances <- varImp(dt_model)
importances

# Plot the decision tree model
rpart.plot(dt_model)

# r plot for decision tree
rpart.plot(dt_model, extra = 2, type = 5, cex = 0.5, box.palette = c("green", "skyblue"))
rpart.plot(dt_model, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.5)

# Make predictions on test data
dt_pred <- predict(dt_model, newdata = test_data, type = "class")

# Convert actual_labels to factor with the same levels as dt_pred
actual_labels <- factor(test_data[, ncol(test_data)], levels = levels(factor(dt_pred)))

# Get the confusion matrix and calculate performance metrics
conf_mat <- confusionMatrix(data = dt_pred, reference = actual_labels)

# Print the confusion matrix
conf_mat$table

# Print performance metrics
cat("Accuracy:", round(conf_mat$overall["Accuracy"] * 100, 2), "\n")
cat("Precision:", round(conf_mat$byClass["Precision"], 2), "\n")
cat("Recall:", round(conf_mat$byClass["Recall"], 2), "\n")
cat("F1-score:", round(conf_mat$byClass["F1"], 2), "\n")
cat("Sensitivity:", round(conf_mat$byClass["Sensitivity"], 2), "\n")