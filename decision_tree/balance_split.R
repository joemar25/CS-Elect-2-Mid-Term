# Load required libraries
if (!require(caret)) install.packages("caret")
if (!require(tm)) install.packages("tm")

library(caret)
library(tm)

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

# Create a document term matrix
corpus <- Corpus(VectorSource(balanced_df$Summary))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.95) # Remove sparse terms
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- balanced_df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Create a training and testing data set with only the document-term matrix and sentiment columns
trainIndex <- createDataPartition(y = balanced_df$Sentiment, p = 0.8, list = FALSE)
train_df <- dtm_sentiment[trainIndex, ]
test_df <- dtm_sentiment[-trainIndex, ]

# Train a decision tree model
decision_tree_model <- train(sentiment ~ ., data = train_df, method = "rpart")

# Test the decision tree model
predictions <- predict(decision_tree_model, newdata = test_df)
confusionMatrix(predictions, test_df$Sentiment)
