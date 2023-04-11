# Load required libraries
if (!require(tm)) install.packages("tm")
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
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
n <- min(nrow(positive_samples), nrow(negative_samples))
positive_samples_subset <- positive_samples[sample(nrow(positive_samples), n), ]
negative_samples_subset <- negative_samples[sample(nrow(negative_samples), n), ]

# Combine the samples into a balanced dataset
balanced_df <- rbind(positive_samples_subset, negative_samples_subset)

# Convert Sentiment to a factor with levels "positive" and "negative"
balanced_df$Sentiment <- factor(balanced_df$Sentiment, levels = c("positive", "negative"))

# Shuffle the rows of the balanced dataset
set.seed(123)
balanced_df <- balanced_df[sample(nrow(balanced_df)), ]

# Create a corpus of the text summaries
corpus <- VCorpus(VectorSource(balanced_df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.95) # Remove sparse terms
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- balanced_df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Convert dtm_sentiment to a data frame
dtm_sentiment_df <- as.data.frame(dtm_sentiment)

# Convert the dependent variable to a factor
dtm_sentiment_df[, ncol(dtm_sentiment_df)] <- as.factor(dtm_sentiment_df[, ncol(dtm_sentiment_df)])

# Define cross-validation settings
train_control <- trainControl(method = "cv", number = 5)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(dtm_sentiment_df), nrow(dtm_sentiment_df) * 0.8)
train_data <- dtm_sentiment_df[train_indices, ]
test_data <- dtm_sentiment_df[-train_indices, ]

# Convert the matrix to a TermDocumentMatrix
tdm <- as.TermDocumentMatrix(dtm, weighting = weightTfIdf)

# Select the most relevant independent variables for the model
relevant_vars <- findFreqTerms(tdm, lowfreq = 10)


















# Create the formula for decision tree model
dt_formula <- as.formula(paste("sentiment ~", paste(relevant_vars, collapse = " + ")))

# Train the decision tree model using caret::train()
dt_model <- train(dt_formula, data = train_data, method = "rpart",
                  control = rpart.control(minsplit = 2), trControl = train_control,
                  tuneLength = 5)
