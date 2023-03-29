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
df <- read.csv("combined_data_for_sentiment.csv", stringsAsFactors = FALSE)
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







# folding
library(caret)

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

conf_mat_table <- table(nb_pred, dtm_sentiment_df[, ncol(dtm_sentiment_df)])
conf_mat_table

accuracy <- conf_mat$overall["Accuracy"] * 100 # Multiply by 100 to get percentage
accuracy <- round(accuracy, 2)
precision <- conf_mat$byClass["Precision"]
recall <- conf_mat$byClass["Recall"]
f1_score <- conf_mat$byClass["F1"]
sensitivity <- conf_mat$byClass["Sensitivity"]

# Evaluate model performance on test data
# Print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1-score:", round(f1_score, 2), "\n")
cat("Sensitivity:", round(sensitivity, 2), "\n")












# decision tree after the model
# Convert sentiment labels to factor
train_data$sentiment <- factor(train_data$sentiment)

# Build decision tree
tree_model <- rpart(sentiment ~ ., data = train_data)

# Plot decision tree
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5)
rpart.plot(tree_model, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.6)










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
