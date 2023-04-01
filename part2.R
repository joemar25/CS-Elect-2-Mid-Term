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
library(dplyr)

library(e1071)




# Load the dataset from flipkart
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)

# Load the model from the file
nb_model <- readRDS("nb_model.rds")

summary(df)


#get neutral sentiments to make them positive or negative
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
  neutral_no_more[i, names(neutral_samples)] <- as.character(neutral_samples[i, ])
  neutral_no_more[i, "Predicted_Sentiment"] <- sentiment_labels[new_pred]
  
  # Print predicted sentiment label
  #cat("Predicted sentiment label for sample", i, ":", sentiment_labels[new_pred], "\n")
}

# Remove the "Sentiment" column from neutral_no_more
neutral_no_more <- subset(neutral_no_more, select = -Sentiment)

# Rename the "Predicted_Sentiment" column to "Sentiment" in neutral_no_more
neutral_no_more <- neutral_no_more %>%
  rename(Sentiment = Predicted_Sentiment)








# Split the data by sentiment
positive_samples <- df[df$Sentiment == "positive", ]
negative_samples <- df[df$Sentiment == "negative", ]


# Set a random seed for reproducibility
set.seed(123)

# Randomly permute the row indices of each data frame
positive_samples <- positive_samples[sample(nrow(positive_samples)), ]
negative_samples <- negative_samples[sample(nrow(negative_samples)), ]
neutral_no_more <- neutral_no_more[sample(nrow(neutral_no_more)), ]

# Combine the three data frames into one larger data frame
all_samples <- rbind(positive_samples, negative_samples, neutral_no_more)











# Set a random seed for reproducibility
set.seed(123)

# Generate a random permutation of row indices
perm <- sample(nrow(all_samples))

# Reorder the rows of the data frame using the permutation
all_samples <- all_samples[perm, ]

summary(all_samples)







































# product management
# replace df with all_sample
# clean the ProductName since it has some values like â

# Remove duplicates and standardize the text data
all_samples$ProductName <- tolower(all_samples$ProductName)
all_samples$ProductName <- trimws(all_samples$ProductName)
all_samples <- distinct(all_samples, ProductName, .keep_all = TRUE)

# Extract only the ProductName column
df_products <- data.frame(all_samples$ProductName)

# Add columns for overall_rating, price, negative_count, and positive_count
df_products <- df_products %>% 
  mutate(overall_rating = NA, price = NA, negative_count = NA, positive_count = NA)

























