# *************** Data Mining Mid Term **************** #
#                                                       #
#                       Title                           #
#                                                       #
#                   Group Members                       #
#             Altamera, Kristel Suzeth                  #
#                   Cardiño, Joemar                     #
#                Enorme, Karl Cedric                    #
#                   Saba, Ainah                         #
#             San Joaquin, Gabriel Jean                 #
#                                                       #
# ***************************************************** #


# ***************************************************** #
# Our FOCUS is Finance & Ecommerce                      #
# IDENTIFY: what is the business tasks?                 #
#           what is the problem?                        #
#           what needed to solve it?                    #
# TASK:                                                 #
# - check kung biased ba ung dataset or hindi           #
# - idenfy kung ilan at ano ang mga items               #
# - icategorize kung ilan ito tas bilang ulit           #
#                                                       #
# - based sa items na nasa dataset - i gogroup ito;     #
# - tas kunin ung total sentiment analysis para dito    #
#   sa group items                                      #
# - so makiktia kung ung item ba ay                     #
#   positive/negative/neutral para sa mga users         #
# - sa part na to, pwedi natin ma conclude or isabi     #
#   ung mga sulit or hindi na items na pwedi bilhin     #
#   dito sa shop                                        #
# - ung negative sentiment saka positive bilangin ung   #
#   total price para sakanila, tignan kung sulit nga    #
#   ba ung pagkabili                                    #
# - tas ung overall total                               #
# - the most negative item(s) that shown here to the    #
#   company act. thus only sell good products (gaya sa  #
#   lazada/shoppee) not all items are good since may    #
#   mga scam jan. so para dina mabili ung item pwedi    #
#   na alisin ung product na puro negative ang review   #
#   or anuman                                           #
#                                                       #
# - sa nakita na may pinaka madami na item na           #
#   positive - pwedi ito magamit as advertisement sa    #
#   site                                                #
# - understand customers buying habbits (if possible)   #
# - alin ung top item sa company na pedi i advertised   #
#                                                       #
# - train using the dataset to identify the sentiment   #
#                                                       #
# NOTE                                                  #
# - tanong tayo ng madami para mas malaman ung problem  #
#   saka goal                                           #
#                                                       #
# ***************************************************** #










# ****************** Data Exploration ***************** #

# install required packages and Use Library
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# load the data from the CSV file
df <- read.csv("dataset.csv")

# set the width of the console to 120 characters
options(width = 120)

# check the structure of the data frame
str(df)

# checking dataset of each columns, and we can see the columns if has missing values
summary(df)

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

# ******************** The Sentiment ****************** #

unique(df$Sentiment)

sentiment_counts <- df %>% count(Sentiment)
sentiment_counts$total <- obs_count
sentiment_counts$Percentage <- paste0(round((sentiment_counts$n / sentiment_counts$total) * 100, 2), "%")
sentiment_counts

# plot start
options("scipen" = 100, "digits" = 4)

# this is not case sensitive (positive can also be Positive)
df_bar <- data.frame(
  Sentiment = c("Positive", "Negative", "Neutral"),
  Count = c(sentiment_counts$n[sentiment_counts$Sentiment == "positive"], sentiment_counts$n[sentiment_counts$Sentiment == "negative"], sentiment_counts$n[sentiment_counts$Sentiment == "neutral"])
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
top_3_df$Percentage <- format(round(as.numeric(sub("%", "", top_3_df$Percentage)) / 100, 2), nsmall = 2, digits = 2)

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
formatted_price <- paste0("$", format(total_price, big.mark = ",", scientific = FALSE))

# print the formatted total price
formatted_price # this is suppoed to be $895,321,820 when data is cleaned for ProductPrice

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





# Summary statistics for Review length
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



























# ****************** Data Pre-Process ***************** #

# start
df <- read.csv("dataset.csv")

# checking
summary(df)

# Check for missing values
sum(is.na(df))

# *********************** General ********************* #

# rename the kzProductName column to ProductName
colnames(df)[colnames(df) == "kzProductName"] <- "ProductName"

# verify that the column has been renamed
colnames(df)



# ****************# The ProductName #****************** #










# ********************* The Price ********************* #

# define a regular expression to extract numeric values from strings
pattern <- "\\d+(\\.\\d+)?"

# clean the ProductPrice column by extracting numeric values
# convert the value to double
df$ProductPrice <- sapply(df$ProductPrice, function(x) {
  ifelse(grepl(pattern, x), as.double(regmatches(x, regexpr(pattern, x))), NA)
})

# check for missing values and inspect corresponding rows
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
cat("Valid:", num_valid, "\n")
cat("Mismatched:", num_mismatched, "\n")
cat("Missing:", num_missing, "\n")
cat("Mean:", mean_price, "\n")
cat("Std. Deviation:", sd_price, "\n")

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



# ********************* Other Col ********************* #

# itama ang mga data types ng columns
# linisin ang mga columns data
