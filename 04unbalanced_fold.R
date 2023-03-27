# Suppress warning messages
options(warn = -1)

if (!require(tm)) install.packages("tm")
if (!require(slam)) install.packages("slam")
if (!require(e1071)) install.packages("e1071")
if (!require(rpart)) install.packages("rpart")

library(tm)
library(tidytext)
library(slam)
library(rpart)
library(rpart.plot)

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

# Fit the decision tree model
tree_model <- rpart(sentiment ~ ., data = dtm_sentiment_df, method = "class")
selected_features <- as.character(rownames(as.data.frame(summary(tree_model)$importance[, 4] > 0)))
dtm_subset <- dtm[, selected_features] # Subset dtm using selected features
dtm_sentiment <- cbind(dtm_subset, sentiment) # Combine subset dtm with sentiment column


# r plot for decision tree (for balanced clean data)
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5)
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5, box.col = "transparent", border = "transparent")


rpart.plot(tree_model, uniform = TRUE, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.6)

# r plot for decision tree (for balanced clean data) without node boxes and plot border
rpart.plot(tree_model,
           uniform = TRUE, extra = 2, fallen.leaves = FALSE,
           type = 5, cex = 0.6, box.col = "transparent", border = "transparent"
)







# welcome to folding
# Define the number of folds
k <- 5

# Create a vector of row indices
indices <- sample(rep(1:k, length.out = nrow(dtm_sentiment_df)))

# Initialize a vector to store the evaluation metrics
accuracy_vec <- numeric(k)
precision_vec <- numeric(k)
recall_vec <- numeric(k)
f1_score_vec <- numeric(k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Split data into training and testing sets
  test_data <- dtm_sentiment_df[indices == i, ]
  train_data <- dtm_sentiment_df[indices != i, ]
  
  # Train the Naive Bayes model
  nb_model <- naiveBayes(x = train_data[, 1:(ncol(train_data) - 1)], y = train_data[, ncol(train_data)])
  
  # Make predictions on test data
  nb_pred <- predict(nb_model, newdata = test_data[, 1:(ncol(test_data) - 1)])
  
  # Evaluate model performance on test data
  conf_mat <- table(nb_pred, test_data[, ncol(test_data)])
  
  accuracy_vec[i] <- sum(diag(conf_mat)) / sum(conf_mat) * 100 # Multiply by 100 to get percentage
  accuracy_vec[i] <- round(accuracy_vec[i], 2)
  precision_vec[i] <- diag(conf_mat) / colSums(conf_mat)
  recall_vec[i] <- diag(conf_mat) / rowSums(conf_mat)
  f1_score_vec[i] <- 2 * precision_vec[i] * recall_vec[i] / (precision_vec[i] + recall_vec[i])
}

# Calculate the mean evaluation metrics across the folds
accuracy <- mean(accuracy_vec)
precision <- mean(precision_vec)
recall <- mean(recall_vec)
f1_score <- mean(f1_score_vec)










# decision tree after the model
# Convert sentiment labels to factor
train_data$sentiment <- factor(train_data$sentiment)

# Build decision tree
tree_model <- rpart(sentiment ~ ., data = train_data)

# Plot decision tree
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5)
rpart.plot(tree_model, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.6)







# Evaluate model performance on test data
# Print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1-score:", round(f1_score, 2), "\n")








# testing the model

# Define the text labels
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





# Save the model to a file
# saveRDS(nb_model, file = "nb_model.rds")
