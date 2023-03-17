# Naive Bayes


# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]
df$Sentiment <- factor(df$Sentiment, levels = c("positive", "negative"))


# Suppress warning messages
options(warn = -1)

# Load the required packages
library(slam)
library(e1071)
library(tm)

# Training
# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]

# Split the data by sentiment
positive_samples <- df[df$Sentiment == "positive", ]
negative_samples <- df[df$Sentiment == "negative", ]

# Sample n rows from the positive and negative samples
n <- min(nrow(positive_samples), nrow(negative_samples))
positive_samples_subset <- positive_samples[sample(nrow(positive_samples), n), ]
negative_samples_subset <- negative_samples[sample(nrow(negative_samples), n), ]

# Combine the samples into a balanced dataset
balanced_df <- rbind(positive_samples_subset, negative_samples_subset)

# Shuffle the rows of the balanced dataset
balanced_df <- balanced_df[sample(nrow(balanced_df)), ]
balanced_df$Sentiment <- factor(balanced_df$Sentiment, levels = c("positive", "negative"))

# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(balanced_df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.99) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- balanced_df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(dtm_sentiment), nrow(dtm_sentiment) * 0.8)
train_data <- dtm_sentiment[train_indices, ]
test_data <- dtm_sentiment[-train_indices, ]

# Train the Naive Bayes model
nb_model <- naiveBayes(x = train_data[, -ncol(train_data)], y = train_data[, ncol(train_data)])

# Make predictions on test data
nb_pred <- predict(nb_model, newdata = test_data[, -ncol(test_data)])

# Evaluate model performance on test data
conf_mat <- table(nb_pred, test_data[, ncol(test_data)])
conf_mat

accuracy <- sum(diag(conf_mat)) / sum(conf_mat) * 100 # Multiply by 100 to get percentage
accuracy <- round(accuracy, 2)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
f1_score <- 2 * precision * recall / (precision + recall)

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
saveRDS(nb_model, file = "nb_model.rds")
