# Naive Bayes


# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]
df$Summary <- as.factor(df$Summary)

# Suppress warning messages
options(warn=-1)

# Load the required packages
library(slam)
library(e1071)
library(tm)

# further analysis
# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]
df$Summary <- as.factor(df$Summary)




# Count the number of summaries in each sentiment category
summary_counts <- table(df$Sentiment)
summary_counts

issame_with_obs <- sum(summary_counts)
issame_with_obs

sentiment_counts <- df %>% count(Sentiment)
sentiment_counts$total <- issame_with_obs
sentiment_counts$Percentage <- paste0(
  round((sentiment_counts$n / sentiment_counts$total) * 100, 2),
  "%"
)
sentiment_counts

# plot start
options("scipen" = 100, "digits" = 4)

# this is not case sensitive (positive can also be Positive)
df_bar <- data.frame(
  Sentiment = c("Positive", "Negative", "Neutral"),
  Count = c(
    sentiment_counts$n[sentiment_counts$Sentiment == "positive"],
    sentiment_counts$n[sentiment_counts$Sentiment == "negative"],
    sentiment_counts$n[sentiment_counts$Sentiment == "neutral"]
  )
)

ggplot(df_bar, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Sentiment Analysis") +
  xlab("Sentiment") +
  ylab("Count")

# Convert the text summaries to a corpus for further analysis
corpus <- Corpus(VectorSource(df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.95) # Remove sparse terms (allocation of memory)

# Get the most frequent words in each sentiment category
findFreqTerms(dtm, lowfreq = 50) # Change the value of "lowfreq" as needed








# Training
# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]
df$Summary <- as.factor(df$Summary)

# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.95) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(dtm_sentiment), nrow(dtm_sentiment) * 0.8)
train_data <- dtm_sentiment[train_indices, ]
test_data <- dtm_sentiment[-train_indices, ]
test_data
# Train the Naive Bayes model
nb_model <- naiveBayes(x = train_data[, -ncol(train_data)], y = train_data[, ncol(train_data)])

# Make predictions on test data
nb_pred <- predict(nb_model, newdata = test_data[, -ncol(test_data)])

# Evaluate model performance on test data
conf_mat <- table(nb_pred, test_data[, ncol(test_data)])
conf_mat

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1-score:", round(f1_score, 2), "\n")






# testing

# Preprocess new text
new_text <- "bad product"
new_corpus <- Corpus(VectorSource(new_text))
new_dtm <- DocumentTermMatrix(new_corpus, control = list(stopwords = TRUE, minDocFreq = 10))
new_dtm <- removeSparseTerms(new_dtm, 0.99)
new_dtm <- as.matrix(new_dtm)

# Predict sentiment of new text
new_pred <- predict(nb_model, newdata = new_dtm)

# Print predicted sentiment label
print(new_pred)









