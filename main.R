# ******************* PRE-PROCESSING ********************






# (1) Install required packages if not already installed

if(!require(ISLR)) 
  install.packages("ISLR")

if(!require(ggplot2))
  install.packages("ggplot2")

if(!require(wordcloud))
  install.packages("wordcloud")

if(!require(tm))
  install.packages("tm")

if(!require(quanteda))
  install.packages("quanteda")





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




# (4) Explore Data

# (a) View the rows of the data-set & see the rows to be viewed
head(df)
head(df, 3)

# (b) how many rows
nrow(df)
ncol(df)
colnames(df)
df$Summary

num_pos <- sum(df$Sentiment == "positive")
num_neg <- sum(df$Sentiment == "negative")
num_neu <- sum(df$Sentiment == "neutral")

cat("number of sentiment positive: ", num_pos, "\n")
cat("number of sentiment negative: ", num_neg, "\n")
cat("number of sentiment neutral : ", num_neu, "\n")

options("scipen"=100, "digits"=4)

df_bar <- data.frame(
  Sentiment = c("Positive", "Negative", "Neutral"),
  Count = c(num_pos, num_neg, num_neu)
)

ggplot(df_bar, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Sentiment Analysis") +
  xlab("Sentiment") +
  ylab("Count")

# (b) plot word cloud of most frequent words
corpus <- Corpus(VectorSource(df$Summary))
corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stop words
corpus <- tm_map(corpus, stemDocument) # stem words
dtm <- DocumentTermMatrix(corpus) # create document-term matrix
freq <- colSums(as.matrix(dtm)) # compute frequency of each word
top <- sort(freq, decreasing = TRUE)[1:10] # get top 10 most frequent words
top

