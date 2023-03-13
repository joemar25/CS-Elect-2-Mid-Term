# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)

# Load the required packages
library(slam)
library(e1071)

# Convert the Sentiment column to a factor
df$Sentiment <- as.factor(df$Sentiment)

# Create a document term matrix
corpus <- Corpus(VectorSource(df$Summary))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.95) # Remove sparse terms
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- df$Sentiment
trainData <- cbind(dtm, sentiment)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(trainData), nrow(trainData) * 0.8)
trainData <- trainData[train_indices, ]
testData <- trainData[-train_indices, ]

# Train the SVM model
svm_model <- svm(x = trainData[, -ncol(trainData)], y = trainData[, ncol(trainData)], kernel = "linear")

# Make predictions on test data
svm_pred <- predict(svm_model, newdata = testData[, -ncol(testData)])

# Evaluate model performance
conf_mat <- table(svm_pred, testData[, ncol(testData)])
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1-score:", round(f1_score, 2), "\n")
