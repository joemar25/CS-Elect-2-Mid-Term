# load the data from the CSV file
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
