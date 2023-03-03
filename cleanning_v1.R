if (!require(ISLR)) {
  install.packages("ISLR")
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
}

if (!require(wordcloud)) {
  install.packages("wordcloud")
}

if (!require(tm)) {
  install.packages("tm")
}

if (!require(quanteda)) {
  install.packages("quanteda")
}


library(ISLR)
library(ggplot2)
library(wordcloud)
library(tm)
library(quanteda)

# load the data from the CSV file
df <- read.csv("dataset.csv")

# check the structure of the data frame
str(df)

# (a) View the rows of the data-set & see the rows to be viewed
head(df)
head(df, 3)

# (b) how many rows
nrow(df)
ncol(df)
colnames(df)

head(df$Summary, 10)

num_pos <- sum(df$Sentiment == "positive")
num_neg <- sum(df$Sentiment == "negative")
num_neu <- sum(df$Sentiment == "neutral")

cat("number of sentiment positive: ", num_pos, "\n")
cat("number of sentiment negative: ", num_neg, "\n")
cat("number of sentiment neutral : ", num_neu, "\n")

options("scipen" = 100, "digits" = 4)

df_bar <- data.frame(
  Sentiment = c("Positive", "Negative", "Neutral"),
  Count = c(num_pos, num_neg, num_neu)
)

ggplot(df_bar, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Sentiment Analysis") +
  xlab("Sentiment") +
  ylab("Count")









# Load the necessary libraries
library(stringr)
library(dplyr)
library(tm)

# Load the data from the CSV file
df <- read.csv("dataset.csv")

# Select the relevant columns for sentiment analysis
df_clean <- df %>% select(Summary, Sentiment)

# Remove punctuation, special characters, and numbers
df_clean$Summary <- gsub("[^[:alpha:][:space:]]*", "", df_clean$Summary)

# Convert text to lowercase
df_clean$Summary <- tolower(df_clean$Summary)

# Remove extra white spaces from the Summary column
df_clean$Summary <- str_replace_all(trimws(df_clean$Summary), "\\s+", " ")

# Define the text pre-processing steps
preproc <- content_transformer(function(x) {
  x <- removePunctuation(x)
  x <- tolower(x)
  x <- stripWhitespace(x)
  return(x)
})

# Apply the text pre-processing to the Summary column
corpus <- Corpus(VectorSource(df_clean$Summary))
corpus_clean <- tm_map(corpus, preproc)
df_clean$Summary <- sapply(corpus_clean, as.character)

# Preview the cleaned data
head(df_clean$Summary, 50)
