# ****************** Data Exploration ***************** #

# (1) Install required packages if not already installed

if (!require(ISLR)) install.packages("ISLR")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(wordcloud)) install.packages("wordcloud")
if (!require(tm)) install.packages("tm")
if (!require(quanteda)) install.packages("quanteda")

# (2) Import the libraries to be used

library(ISLR)
library(ggplot2)
library(wordcloud)
library(tm)
library(quanteda)

# (3) get file from the correct directory

# load the data from the CSV file
df <- read.csv("dataset.csv")

# check the structure of the data frame
str(df)

# obs and var
obs_count <- nrow(df)
var_count <- ncol(df)
colm_name <- colnames(df)

cat("obs. or rows->", obs_count)
cat("var. or cols->", var_count)
cat("column      ->", colm_name)

# (4) Explore Data

# (a) View the rows of the data-set (5, since it is hard to return all data)
head(df, 5)

# present rows value of each column temporarily 
head(df$kzProductName, 5)
head(df$ProductPrice,  5)
head(df$Rate,          5)
head(df$Review,        5)
head(df$Summary,       5)
head(df$Sentiment,     5)


# (b) identify number of positive/negative/neutral

num_pos <- sum(df$Sentiment == "positive")
num_neg <- sum(df$Sentiment == "negative")
num_neu <- sum(df$Sentiment == "neutral")
total <- num_pos + num_neg + num_neu

cat("number of sentiment positive: ", num_pos, "\n")
cat("number of sentiment negative: ", num_neg, "\n")
cat("number of sentiment neutral : ", num_neu, "\n")

# nani-kore
cat("number of sentiment values  : [", total,   "] and obs. [", obs_count, "]\n")

# - (1) why is that the total and obs don't match? - maybe there are null values
sum(is.na(df$Sentiment))

# - (2) why is that the total and obs don't match? - maybe there are not equal to positive/negative/neutral
other_sentiments <- df$Sentiment[!(df$Sentiment %in% c("positive", "negative", "neutral"))]

# truth
print(other_sentiments)

# get the other sentiments count
num_other_sentiments <- sum(!(df$Sentiment %in% c("positive", "negative", "neutral")))
cat("not (positive/negative/neutral) : ", num_other_sentiments, "\n")

total <- num_other_sentiments + total
cat("number of sentiment values  : [", total,   "] and obs. [", obs_count, "]\n")




# plot
options("scipen" = 100, "digits" = 4)

# this is not case sensitive (positive can also be Positive)
df_bar <- data.frame(
    Sentiment = c("Positive", "Negative", "Neutral"),
    Count = c(num_pos, num_neg, num_neu)
)

ggplot(df_bar, aes(x = Sentiment, y = Count, fill = Sentiment)) +
    geom_bar(stat = "identity") +
    ggtitle("Sentiment Analysis") +
    xlab("Sentiment") +
    ylab("Count")



# kzProductName

# Create a frequency table of the kzProductName column
freq_table <- table(df$kzProductName)

# Sort the table in descending order
sorted_table <- sort(freq_table, decreasing = TRUE)

# Select the top 3 values | determine most common word to occur
top_3_values <- head(sorted_table, n = 3)
print(top_3_values)










# Price

# Convert ProductPrice to numeric type
df$ProductPrice <- as.numeric(df$ProductPrice)

# Count the number of missing values in ProductPrice
num_missing <- sum(is.na(df$ProductPrice))

# Count the number of non-missing values in ProductPrice
num_valid <- sum(!is.na(df$ProductPrice))

# Compute the number of values that don't match the numeric format
num_mismatched <- sum(is.na(as.numeric(df$ProductPrice)))

# Compute the mean and standard deviation of ProductPrice (excluding missing and mismatched values)
mean_price <- mean(df$ProductPrice[!is.na(df$ProductPrice) & !is.na(as.numeric(df$ProductPrice))])
sd_price <- sd(df$ProductPrice[!is.na(df$ProductPrice) & !is.na(as.numeric(df$ProductPrice))])

# Print the results
cat("Valid:", num_valid, "\n")
cat("Mismatched:", num_mismatched, "\n")
cat("Missing:", num_missing, "\n")
cat("Mean:", mean_price, "\n")
cat("Std. Deviation:", sd_price, "\n")