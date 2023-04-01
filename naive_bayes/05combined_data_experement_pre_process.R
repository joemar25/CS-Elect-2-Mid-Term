# Suppress warning messages
options(warn = -1)

if (!require(tm)) install.packages("tm")
if (!require(textstem)) install.packages("textstem")
if (!require(slam)) install.packages("slam")

library(tm)
library(tidytext)
library(textstem)





# amazon cells with labelled sentiment (1k dataset) (unclean)

# Load the dataset from a text file with tabs as separator
amazon_ds <- read.delim("amazon_cells_labelled.txt", header=FALSE, sep="\t", quote="")

# Rename columns of the dataframe
names(amazon_ds)[1] <- "Summary"
names(amazon_ds)[2] <- "Sentiment"

# View the first few rows of the dataset ; (0,1) is present
head(amazon_ds)

# Replace values in the "Sentiment" column with labels
amazon_ds$Sentiment <- ifelse(amazon_ds$Sentiment == 1, "positive", "negative")

# Check the unique values in the "Sentiment" column
unique(amazon_ds$Sentiment)


# clean amazon_df -> Summary

# Clean the Summary column
corpus <- Corpus(VectorSource(amazon_ds$Summary))

# Apply text cleaning steps
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower)) # Add lowercase transformation
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, function(x) lemmatize_strings(x, pos = "all"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(trimws)) # Add trimws function to remove leading/trailing whitespace

# Replace the original column with the cleaned data
amazon_ds$Summary <- unlist(sapply(corpus, as.character))

# Check the cleaned data
head(amazon_ds$Summary, 5)





# warning - this is a very heavy dataset (3,599,999 for train)

# Amazon reviews polarity dataset (trian)

arp_df <- read.csv("train.csv", stringsAsFactors = FALSE)

# we will transform this "X2" to "Sentiment" and "Stuning.even.for.the.non.gamer" to "Summary"
summary(arp_df)

arp_df <- arp_df[, c("X2", "Stuning.even.for.the.non.gamer")]
summary(arp_df)


# Rename the columns
names(arp_df) <- c("Sentiment", "Summary")

arp_df <- arp_df[, c("Summary", "Sentiment")]

summary(arp_df)

unique(arp_df$Sentiment)

# Replace values in the "Sentiment" column with labels (2 here is positive. 1 is negative)
arp_df$Sentiment <- ifelse(arp_df$Sentiment == 2, "positive", "negative")

unique(arp_df$Sentiment)



# clean Amazon reviews polarity -> Summary

# Clean the Summary column
corpus <- Corpus(VectorSource(arp_df$Summary))

# Apply text cleaning steps
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower)) # Add lowercase transformation
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, function(x) lemmatize_strings(x, pos = "all"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(trimws)) # Add trimws function to remove leading/trailing whitespace

# Replace the original column with the cleaned data
arp_df$Summary <- unlist(sapply(corpus, as.character))

# Check the cleaned data
head(arp_df$Summary, 5)








# warning - this is a very heavy dataset (399,999 for test)

# Amazon reviews polarity dataset (test)

arp_dft <- read.csv("test.csv", stringsAsFactors = FALSE)

# we will transform this "X2" to "Sentiment" and "Great.CD" to "Summary"
summary(arp_dft)

arp_dft <- arp_dft[, c("X2", "Great.CD")]
summary(arp_dft)


# Rename the columns
names(arp_dft) <- c("Sentiment", "Summary")

arp_dft <- arp_dft[, c("Summary", "Sentiment")]

summary(arp_dft)

unique(arp_dft$Sentiment)

# Replace values in the "Sentiment" column with labels (2 here is positive. 1 is negative)
arp_dft$Sentiment <- ifelse(arp_dft$Sentiment == 2, "positive", "negative")

unique(arp_dft$Sentiment)



# clean Amazon reviews polarity -> Summary

# Clean the Summary column
corpus <- Corpus(VectorSource(arp_dft$Summary))

# Apply text cleaning steps
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower)) # Add lowercase transformation
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, function(x) lemmatize_strings(x, pos = "all"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(trimws)) # Add trimws function to remove leading/trailing whitespace

# Replace the original column with the cleaned data
arp_dft$Summary <- unlist(sapply(corpus, as.character))

# Check the cleaned data
head(arp_dft$Summary, 5)








# main dataset (cleaned)

# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
df <- df[, c("Summary", "Sentiment")]


# Split the data by sentiment
positive_samples <- df[df$Sentiment == "positive", ]
negative_samples <- df[df$Sentiment == "negative", ]


# Sample n rows from the positive and negative samples
positive_samples_subset <- positive_samples[sample(nrow(positive_samples), ), ]
negative_samples_subset <- negative_samples[sample(nrow(negative_samples), ), ]


# Combine the samples into a unbalanced dataset
unbalanced_df <- rbind(positive_samples_subset, negative_samples_subset)

# Shuffle the rows of the unbalanced dataset
unbalanced_df <- unbalanced_df[sample(nrow(unbalanced_df)), ]
unbalanced_df$Sentiment <- factor(unbalanced_df$Sentiment, levels = c("positive", "negative"))




# Combine the data frames into one
combined_df <- rbind(amazon_ds, arp_df, arp_dft, unbalanced_df)

# Write the combined data frame to a CSV file
write.csv(combined_df, "combined_data_for_sentiment.csv", row.names = FALSE)












# test if file is ok
df <- read.csv("combined_data_for_sentiment.csv", stringsAsFactors = FALSE)
summary(df)
