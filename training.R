
install.packages("dplyr")
install.packages("tidytext")
library(tidytext)
library(dplyr)


# Load the cleaned data
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)

summary(df)

tidy_data <- df %>%
  unnest_tokens(word, Summary)

sentiment_lexicon <- get_sentiments("afinn")

sentiment_data <- tidy_data %>%
  inner_join(sentiment_lexicon, by = "word") %>%
  group_by(Sentiment) %>%
  summarize(sentiment_score = sum(value)) %>%
  ungroup()

library(ggplot2)

ggplot(sentiment_data, aes(x = Sentiment, y = sentiment_score)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Analysis", x = "Sentiment", y = "Sentiment Score")
