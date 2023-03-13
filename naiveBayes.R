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

# Create a document term matrix
corpus <- Corpus(VectorSource(df$Summary))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.99) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix
dtm
# Add sentiment to the matrix
sentiment <- df$Sentiment
trainData <- cbind(dtm, sentiment)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(trainData), nrow(trainData) * 0.8)
trainData <- trainData[train_indices, ]
testData <- trainData[-train_indices, ]

# Train the Naive Bayes model
nb_model <- naiveBayes(x = trainData[, -ncol(trainData)], y = trainData[, ncol(trainData)])

# Make predictions on test data
nb_pred <- predict(nb_model, newdata = testData[, -ncol(testData)])

# Evaluate model performance
table(nb_pred, testData[, ncol(testData)])

# Calculate accuracy, precision, recall, and F1-score
conf_mat <- table(nb_pred, testData[, ncol(testData)])
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1-score:", round(f1_score, 2), "\n")

# further analysis
