# *************** Data Mining Mid Term **************** #
#                                                       #
#                   Group Members                       #
#             Altamera, Kristel Suzeth                  #
#                   Cardiño, Joemar                     #
#                Enorme, Karl Cedric                    #
#                   Saba, Ainah                         #
#             San Joaquin, Gabriel Jean                 #
#                                                       #
# ***************************************************** #


# ****************** Data Exploration ***************** #

# install required packages and Use Library
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(textstem)) install.packages("textstem")

library(ggplot2)
library(dplyr)
library(textstem)




# load the data from the CSV file
df <- read.csv("dataset.csv")

# set the width of the console to 120 characters
options(width = 120)

# check the structure of the data frame
str(df)

# checking dataset of each columns
# we can see the columns if has missing values
summary(df)

head(df$kzProductName, 5)
head(df$Summary, 5)

# obs and var
obs_count <- nrow(df)
var_count <- ncol(df)
colm_name <- colnames(df)
cat("obs. or rows->", obs_count, "\n")
cat("var. or cols->", var_count, "\n")
cat("column      ->", colm_name, "\n")

# explore sample of the data (5 only since data is large)
head(df, 5)

# present rows value of each column temporarily
head(df$kzProductName, 5)
head(df$ProductPrice, 5)
head(df$Rate, 5)
head(df$Review, 5)
head(df$Summary, 5)
head(df$Sentiment, 5)




# ******************** Decision Tree ****************** #
# before pre-processing

# Load required library
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(textstem)) install.packages("textstem")

library(tm)
library(rpart)
library(rpart.plot)




# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.99) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Convert dtm_sentiment to a data frame
dtm_sentiment_df <- as.data.frame(dtm_sentiment)

# Fit the decision tree model
tree_model <- rpart(sentiment ~ ., data = dtm_sentiment_df, method = "class")
selected_features <- as.character(rownames(as.data.frame(summary(tree_model)$importance[,4] > 0)))
dtm_subset <- dtm[, selected_features] # Subset dtm using selected features
dtm_sentiment <- cbind(dtm_subset, sentiment) # Combine subset dtm with sentiment column

# r plot for decision tree (for balanced clean data)
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5)
rpart.plot(tree_model, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.5)






# ******************** The Sentiment ****************** #

unique(df$Sentiment)

sentiment_counts <- df %>% count(Sentiment)
sentiment_counts$total <- obs_count
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

# ****************** The kzProductName **************** #

# Create a frequency table of the kzProductName column
freq_table <- table(df$kzProductName)

# Sort the table in descending order
sorted_table <- sort(freq_table, decreasing = TRUE)

# Select the top 3 values
top_3_values <- head(sorted_table, n = 3)

# Calculate total number of values in the column
total <- sum(freq_table)

# Calculate percentage for each of the top 3 values
percentages <- round((top_3_values / total) * 100, 2)

# Calculate the percentage of all other values
other_percentage <- 100 - sum(percentages)

# Combine the values and percentages into a data frame
top_3_df <- data.frame(
  kzProductName = c(names(top_3_values), "Others"),
  Frequency = c(top_3_values, sum(freq_table) - sum(top_3_values)),
  Percentage = c(paste0(percentages, "%"), paste0(other_percentage, "%"))
)

# Format the output to use fewer decimal places
top_3_df$Percentage <- format(
  round(as.numeric(sub("%", "", top_3_df$Percentage)) / 100, 2),
  nsmall = 2,
  digits = 2
)

# Print the resulting data frame
print(top_3_df, row.names = FALSE)

# ********************* The Price ********************* #

normal_price <- sum(is.na(df$ProductPrice))
num_valid <- sum(!is.na(df$ProductPrice))
num_outlier <- sum(df$ProductPrice < 0)

# Print the results
cat("Number of normal prices: ", normal_price, "\n")
cat("Number of valid prices: ", num_valid, "\n")
cat("Number of outlier prices: ", num_outlier, "\n")

# see outlier prices
outlier_prices <- df$ProductPrice[df$ProductPrice < 0]
outlier_prices

# na.rm = TRUE argument tells R to ignore missing values when calculating the sum
# warning is present since it has outliers
df$ProductPrice <- as.double(df$ProductPrice)
total_price <- sum(df$ProductPrice, na.rm = TRUE)

# format the total price with commas and a dollar sign
formatted_price <- paste0(
  "$",
  format(total_price, big.mark = ",", scientific = FALSE)
)

# print the formatted total price
formatted_price # this is suppoed to be $935,082,929 when data is cleaned for ProductPrice

# find the lowest price
lowest_price <- min(df$ProductPrice, na.rm = TRUE)

# find the highest price
highest_price <- max(df$ProductPrice, na.rm = TRUE)

# print the lowest and highest prices
cat("The lowest price is", lowest_price, "\n")
cat("The highest price is", highest_price, "\n")

# remove rows with missing values in ProductPrice
df <- df[!is.na(df$ProductPrice), ]

# define breaks based on available values
breaks <- seq(min(unique(df$ProductPrice)),
  max(unique(df$ProductPrice)),
  length.out = 10
)

# create histogram with customized options
hist(df$ProductPrice,
  breaks = breaks,
  col = "lightblue",
  border = "white",
  main = "Distribution of Product Prices",
  xlab = "Price (in USD)",
  ylab = "Count",
  xlim = c(min(breaks), max(breaks)),
  ylim = c(0, max(hist(df$ProductPrice, breaks = breaks)$counts) * 1.1)
)

# price representation - will be represented again when it is cleaned to see differences

# ********************** The Rate ********************* #

# Summary statistics for Rate
summary(df$Rate)

# see outlier prices
any(df$Rate < 0)

outlier_rate <- df$Rate[df$Rate < 1 | df$Rate > 5]
outlier_rate

# convert to make graphing
df$Rate <- as.numeric(df$Rate)

# Visualize the distribution of Rate using a histogram
ggplot(df, aes(Rate)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(x = "Rating", y = "Count") +
  ggtitle("Distribution of Ratings")

# Calculate the mean rating
mean(df$Rate, na.rm = TRUE)

# Calculate the median rating
median(df$Rate, na.rm = TRUE)

# Calculate the mode rating

if (!require(modeest)) install.packages("modeest")

library(modeest)

mlv(df$Rate)

ggplot(df, aes(y = Rate)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(y = "Rating") +
  ggtitle("Distribution of Ratings")

# ********************** The Review ******************* #

# Summary statistics for Review
summary(df$Review)

# Top 10 most frequent Review
top_review <- df %>%
  count(Review, sort = TRUE) %>%
  head(10)
top_review

# Bar plot of top 10 most frequent Review
ggplot(top_review, aes(x = Review, y = n)) +
  geom_col() +
  xlab("Review") +
  ylab("Count") +
  ggtitle("Top 10 most frequent Review")

# ********************* The Summary ******************* #

# Summary statistics for Summary
summary(df$Summary)

# Top 10 most common summaries
top_summary <- df %>%
  count(Summary, sort = TRUE) %>%
  head(10)
top_summary

# Bar plot of top 10 most frequent Review
ggplot(top_summary, aes(x = Summary, y = n)) +
  geom_col() +
  xlab("Summary") +
  ylab("Count") +
  ggtitle("Top 10 most frequent Summary")

# ***************************************************** #

























# ****************** Data Pre-Process ***************** #

# load necessary libraries for cleaning the text
if (!require(tm)) install.packages("tm")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(textstem)) install.packages("textstem")

library(tm)
library(ggplot2)
library(textstem)




# Read in the dataset
df <- read.csv("dataset.csv", stringsAsFactors = FALSE)

# checking
summary(df)

# Check for missing values
sum(is.na(df))

# *********************** General ********************* #

# remove unnecessary columns
df <- df[c("kzProductName", "ProductPrice", "Rate", "Summary", "Sentiment")]

# rename column product name
colnames(df)[1] <- "ProductName"

# text pre-processing (lowering text)
df$ProductName <- tolower(df$ProductName)
head(df$ProductName, 5)

df$Summary <- tolower(df$Summary)
head(df$Summary, 5)

df$Sentiment <- tolower(df$Sentiment)
head(df$Sentiment, 5)






# Create a corpus from the text data
corpus <- Corpus(VectorSource(df$ProductName))

# Clean and preprocess the text
corpus <- corpus %>%
  tm_map(content_transformer(iconv), to = "ASCII//TRANSLIT") %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(function(x) lemmatize_strings(x, pos = "all"))

# remove single characters from the text
df$ProductName <- gsub("\\b\\w\\b", "", df$ProductName)

# Convert the preprocessed corpus to a plain text document
df$ProductName <- sapply(corpus, as.character)















# ********************* The Price ********************* # 248

# remove non-numeric characters and convert to numeric
df$ProductPrice <- as.double(gsub("[^0-9.]+", "", df$ProductPrice))

# check for missing values
missing_indices <- which(is.na(df$ProductPrice))
df[missing_indices, "ProductPrice"]

num_missing <- sum(is.na(df$ProductPrice))
num_missing

num_mismatched <- sum(is.na(as.double(df$ProductPrice))) - num_missing
num_mismatched

num_valid <- sum(!is.na(df$ProductPrice))
num_valid

# compute the mean and standard deviation of ProductPrice (excluding missing and mismatched values)
mean_price <- mean(df$ProductPrice[!is.na(df$ProductPrice) & !is.na(as.double(df$ProductPrice))])
sd_price <- sd(df$ProductPrice[!is.na(df$ProductPrice) & !is.na(as.double(df$ProductPrice))])

# print the results
print(paste("Valid:", num_valid))
print(paste("Mismatched:", num_mismatched))
print(paste("Missing:", num_missing))
print(paste("Mean:", mean_price))
print(paste("Std. Deviation:", sd_price))

# calculate the total price
total_price <- sum(df$ProductPrice, na.rm = TRUE)

# format the total price with commas and a dollar sign
formatted_price <- paste0("$", format(total_price, big.mark = ",", scientific = FALSE))

# print the formatted total price
formatted_price

# find the lowest price
lowest_price <- min(df$ProductPrice, na.rm = TRUE)

# find the highest price
highest_price <- max(df$ProductPrice, na.rm = TRUE)

# print the lowest and highest prices
cat("The lowest price is", lowest_price, "\n")
cat("The highest price is", highest_price, "\n")

# define breaks based on available values
breaks <- seq(min(unique(df$ProductPrice)),
  max(unique(df$ProductPrice)),
  length.out = 10
)

# create histogram with customized options
hist(df$ProductPrice,
  breaks = breaks,
  col = "lightblue",
  border = "white",
  main = "Distribution of Product Prices",
  xlab = "Price (in USD)",
  ylab = "Count",
  xlim = c(min(breaks), max(breaks)),
  ylim = c(0, max(hist(df$ProductPrice, breaks = breaks)$counts) * 1.1)
)

# ********************* The Rate ********************** #
summary(df$Rate)

# remove non-numeric characters and convert to numeric
df$Rate <- as.numeric(gsub("[^0-9.]+", "", df$Rate))
head(df$Rate, 5)

# Visualize the distribution of Rate using a histogram
ggplot(df, aes(Rate)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(x = "Rating", y = "Count") +
  ggtitle("Distribution of Ratings")

# Calculate the mean rating
mean(df$Rate, na.rm = TRUE)

# Calculate the median rating
median(df$Rate, na.rm = TRUE)

# Calculate the mode rating

if (!require(modeest)) install.packages("modeest")

library(modeest)

mlv(df$Rate)

ggplot(df, aes(y = Rate)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(y = "Rating") +
  ggtitle("Distribution of Ratings")


# ******************* The Summary ********************* #
# check values
head(df$Summary, 5)

# Create a corpus and remove numbers and punctuation
corpus <- Corpus(VectorSource(df$Summary))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Lemmatize the words
corpus <- tm_map(corpus, function(x) lemmatize_strings(x, pos = "all"))

# Remove extra spaces
corpus <- tm_map(corpus, stripWhitespace)

# Replace the original column with the cleaned data
df$Summary <- unlist(sapply(corpus, as.character))

df$Summary

# [need to have a spell corrector for more accuracy]
















# ******************** Decision Tree ****************** #
# Load required library
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(tm)) install.packages("tm")

library(tm)
library(rpart)
library(rpart.plot)




# Create a corpus of the text summaries
corpus <- Corpus(VectorSource(df$Summary))

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minDocFreq = 10))
dtm <- removeSparseTerms(dtm, 0.99) # Remove sparse terms (allocation of memory)
dtm <- as.matrix(dtm) # Convert to matrix

# Add sentiment to the matrix
sentiment <- df$Sentiment
dtm_sentiment <- cbind(dtm, sentiment)

# Convert dtm_sentiment to a data frame
dtm_sentiment_df <- as.data.frame(dtm_sentiment)

# Fit the decision tree model
tree_model <- rpart(sentiment ~ ., data = dtm_sentiment_df, method = "class")
selected_features <- as.character(rownames(as.data.frame(summary(tree_model)$importance[,4] > 0)))
dtm_subset <- dtm[, selected_features] # Subset dtm using selected features
dtm_sentiment <- cbind(dtm_subset, sentiment) # Combine subset dtm with sentiment column

# r plot for decision tree (for balanced clean data)
rpart.plot(tree_model, extra = 2, type = 5, cex = 0.5)
rpart.plot(tree_model, extra = 2, fallen.leaves = FALSE, type = 5, cex = 0.5)



















# cleaned data frame is named `cleaned_df`
write.csv(df, file = "clean_data.csv", row.names = FALSE)

