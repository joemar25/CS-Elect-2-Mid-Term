# Suppress warning messages
options(warn = -1)

# Load required library
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(tm)) install.packages("tm")

library(tm)
library(rpart)
library(rpart.plot)

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
set.seed(123)
balanced_df <- balanced_df[sample(nrow(balanced_df)), ]

# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(balanced_df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.95) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- balanced_df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Convert dtm_sentiment to a data frame
dtm_sentiment_df <- as.data.frame(dtm_sentiment)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(dtm_sentiment_df), size = round(0.8 * nrow(dtm_sentiment_df)), replace = FALSE)
train_data <- dtm_sentiment_df[train_indices, ]
test_data <- dtm_sentiment_df[-train_indices, ]

# Fit the decision tree model on the training data
tree_model <- rpart(sentiment ~ ., data = train_data, method = "class")
selected_features <- as.character(rownames(as.data.frame(summary(tree_model)$importance[,4] > 0)))
dtm_subset <- dtm[, selected_features] # Subset dtm using selected features
dtm_sentiment_train <- cbind(dtm_subset[train_indices, ], sentiment[train_indices]) # Combine subset dtm with sentiment column in training data
dtm_sentiment_test <- cbind(dtm_subset[-train_indices, ], sentiment[-train_indices]) # Combine subset dtm with sentiment column in testing data

# r plot for decision tree (for balanced clean data)
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5, box.palette = c("green", "skyblue"))
rpart.plot(tree_model, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.5)

# Make predictions on the test set
nb_pred <- predict(tree_model, newdata = test_data, type="class")



# Get the confusion matrix and calculate performance metrics
conf_mat <- confusionMatrix(data = nb_pred, reference = test_data$sentiment)
conf_mat

# Get the table of confusion matrix
conf_mat_table <- table(nb_pred, test_data$sentiment)

# Print the confusion matrix (both alternatives)
conf_mat_table
conf_mat$table

# Print performance metrics
cat("Accuracy:", round(conf_mat$overall["Accuracy"] * 100, 2), "\n")
cat("Precision:", round(conf_mat$byClass["Precision"], 2), "\n")
cat("Recall:", round(conf_mat$byClass["Recall"], 2), "\n")
cat("F1-score:", round(conf_mat$byClass["F1"], 2), "\n")
cat("Sensitivity:", round(conf_mat$byClass["Sensitivity"], 2), "\n")
