# Suppress warning messages
options(warn = -1)

library(tm)
library(tidytext)
library(dplyr)
library(purrr)
library(ggplot2)
library(reshape2)

# Load the dataset from flipkart
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)

# Load the model from the file
nb_model <- readRDS("nb_model.rds")

# get neutral sentiments to make them positive or negative
neutral_samples <- df[df$Sentiment == "neutral", ]

# Define the text labels
sentiment_labels <- c("positive", "negative")

# Create an empty data frame to store the results
neutral_no_more <- data.frame(matrix(ncol = ncol(neutral_samples) + 1, nrow = nrow(neutral_samples)))
colnames(neutral_no_more) <- c(names(neutral_samples), "Predicted_Sentiment")

# Loop through all neutral samples
for (i in 1:nrow(neutral_samples)) {
  
  # Preprocess the new summary
  new_summary <- neutral_samples[i, "Summary"]
  new_corpus <- Corpus(VectorSource(new_summary))
  new_dtm <- DocumentTermMatrix(new_corpus, control = list(stopwords = TRUE, minDocFreq = 10))
  new_dtm <- removeSparseTerms(new_dtm, 0.99)
  new_dtm <- as.matrix(new_dtm)
  
  # Predict sentiment of new summary using trained model
  new_pred <- predict(nb_model, newdata = new_dtm)
  
  # Store original columns and predicted sentiment label in the new data frame
  neutral_no_more[i, names(neutral_samples)] <- neutral_samples[i, ]
  neutral_no_more[i, "Predicted_Sentiment"] <- sentiment_labels[new_pred]
}

# Remove the "Sentiment" column from neutral_no_more
neutral_no_more <- select(neutral_no_more, -Sentiment)

# Rename the "Predicted_Sentiment" column to "Sentiment" in neutral_no_more
neutral_no_more <- rename(neutral_no_more, Sentiment = Predicted_Sentiment)

# Split the data by sentiment
positive_samples <- df[df$Sentiment == "positive", ]
negative_samples <- df[df$Sentiment == "negative", ]

# Set a random seed for reproducibility
set.seed(123)

# Randomly permute the row indices of each data frame
positive_samples <- sample_n(positive_samples, nrow(positive_samples))
negative_samples <- sample_n(negative_samples, nrow(negative_samples))
neutral_no_more <- sample_n(neutral_no_more, nrow(neutral_no_more))

# Combine the three data frames into one larger data frame
all <- bind_rows(positive_samples, negative_samples, neutral_no_more)

# Create an empty data frame to store the summary information
summary_df <- data.frame(product_name = character(),
                         max_price = numeric(),
                         total_positive = numeric(),
                         total_negative = numeric(),
                         stringsAsFactors = FALSE)

# For each unique product in the original data frame
for (product_name in unique(all$ProductName)) {
  
  # Subset the data frame to only include the rows for that product
  product_subset <- all[all$ProductName == product_name, ]
  
  # Calculate the maximum price for that product
  max_price <- max(product_subset$ProductPrice)
  
  # Calculate the total number of positive and negative reviews for that product
  total_positive <- sum(product_subset$Sentiment == "positive")
  total_negative <- sum(product_subset$Sentiment == "negative")
  
  # Create a new row in the empty data frame with the values for the product name, max_price, total_positive, and total_negative
  new_row <- data.frame(product_name = product_name,
                        max_price = max_price,
                        total_positive = total_positive,
                        total_negative = total_negative,
                        stringsAsFactors = FALSE)
  
  # Add the new row to the summary data frame
  summary_df <- rbind(summary_df, new_row)
}

# Remove any duplicate rows in the completed data frame
summary_df <- unique(summary_df)

# Print the completed data frame
summary_df





#A BUNCH OF DATA FRAMES pinaglalaruan katulad ng ginawa niya sayo

# Identify and print products with negative sentiments only: wala sila positive
onlyNEG_prods <- subset(summary_df, total_positive == 0 & total_negative > 0)
onlyNEG_prods

library(dplyr) 

#highest to lowest na onlyNEG_prods 
descending_onlyNEG_prods <- arrange(onlyNEG_prods, desc(total_negative))

# Identify and print products with positive sentiments only: wala naman sila negative
onlyPOS_prods <- filter(summary_df, total_negative == 0 & total_positive > 0)
onlyPOS_prods

#highest to lowest na onlyPOS_prods
descending_onlyPOS_prods <- arrange(onlyPOS_prods, desc(total_positive))

#summary arranged - highest to lowest price
summary_hightolow_price <- summary_df %>%
  arrange(desc(max_price))

#summary arranged - highest to lowest positive value
summary_hightolow_POS <- summary_df %>%
  arrange(desc(total_positive))

#summary arranged - highest to lowest negative value
summary_hightolow_NEG <- summary_df %>%
  arrange(desc(total_negative))

#VISUALIZATION

#BAR CHART
# Select the first 5 rows of the summary_df dataframe
summary_head <- head(summary_df, 5)

# Create a bar chart of the total positive and total negative sentiments
ggplot(summary_head, aes(x=product_name, y=total_positive)) +
  geom_bar(stat="identity", fill="green", alpha=0.5) +
  geom_bar(aes(y=total_negative), stat="identity", fill="red", alpha=0.5) +
  xlab("Product Name") +
  ylab("Sentiment Count") +
  ggtitle("bar chart of sentiments")

