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

# Install required packages and Use Library

if (!require(ggplot2)) install.packages("ggplot2")

library(ggplot2)

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

# explore sample of the data

# view the rows of the data-set (5, since it is hard to return all data)
head(df, 5)

# present rows value of each column temporarily
head(df$kzProductName, 5)
head(df$ProductPrice, 5)
head(df$Rate, 5)
head(df$Review, 5)
head(df$Summary, 5)
head(df$Sentiment, 5)



# ******************** The Sentiment ****************** #

# identify number of positive/negative/neutral

num_pos <- sum(df$Sentiment == "positive")
num_neg <- sum(df$Sentiment == "negative")
num_neu <- sum(df$Sentiment == "neutral")
total <- num_pos + num_neg + num_neu

cat("number of sentiment positive: ", num_pos, "\n")
cat("number of sentiment negative: ", num_neg, "\n")
cat("number of sentiment neutral : ", num_neu, "\n")

# nani-kore
cat("number of sentiment values  : [", total, "] and obs. [", obs_count, "]\n")

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
cat("number of sentiment values  : [", total, "] and obs. [", obs_count, "]\n")

# plot start
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



# ****************** The kzProductName **************** #

# Create a frequency table of the kzProductName column
freq_table <- table(df$kzProductName)

# Sort the table in descending order
sorted_table <- sort(freq_table, decreasing = TRUE)

# Select the top 3 values | determine most common word to occur
top_3_values <- head(sorted_table, n = 3)
print(top_3_values)



# ********************* The Price ********************* #

# if the ProductPrice is in character then normal_price would be 0
normal_price <- sum(is.na(df$ProductPrice))
normal_price

# converting to double
df$ProductPrice <- as.double(df$ProductPrice)

# count the number of non-missing values in ProductPrice
num_valid <- sum(!is.na(df$ProductPrice))

# count the number of missing values in ProductPrice
num_missing <- sum(is.na(df$ProductPrice))

# count the number of values that don't match the numeric format
num_mismatched <- sum(is.na(as.double(df$ProductPrice))) - num_missing

# compute the mean and standard deviation of ProductPrice (excluding missing and mismatched values)
mean_price <- mean(df$ProductPrice[!is.na(df$ProductPrice) & !is.na(as.double(df$ProductPrice))])
sd_price <- sd(df$ProductPrice[!is.na(df$ProductPrice) & !is.na(as.double(df$ProductPrice))])

# Print the results
cat("Valid:", num_valid, "\n")
cat("Mismatched:", num_mismatched, "\n")
cat("Missing:", num_missing, "\n")
cat("Mean:", mean_price, "\n")
cat("Std. Deviation:", sd_price, "\n")

# questioning the result. Why are there so many missing values?

# check the totality of valid and missing to the obs.
t <- num_valid + num_missing

cat("valid + missing =", t, "\n")
cat("obs. =", obs_count, "\n")

# ----> the problem is in the num_missing when df$ProductPrice is converted to Double
# value are not cleaned yet, but it convert's the values to double making the invalid numbers NaN

# Present the missing values in ProductPrice
missing_values <- df$ProductPrice[which(is.na(df$ProductPrice))]
missing_values

# calculate the total price
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

# convert ProductPrice to numeric type
df$ProductPrice <- as.double(df$ProductPrice)

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



















# ****************** Data Pre-Process ***************** #

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

























# testing for data pre-processing TESTING LNG POOOO************************
library(caret)


# DATA SPLITTING
# Split data into training and testing subsets
set.seed(123) # set a seed for reproducibility
trainIndexdf <- createDataPartition(df$Sentiment, p = 0.8, list = FALSE)
traindf <- df[trainIndexdf, ]
testdf <- df[-trainIndexdf, ]


# PRE-PROCESSING:
# Pre-process data
preProcdf <- preProcess(traindf[, -5], method = c("center", "scale", "pca"))

# Apply pre-processing to training and testing data
trainTransformeddf <- predict(preProcdf, traindf[, -5])
testTransformeddf <- predict(preProcdf, testdf[, -5])


# Train a predictive model
modeldf <- train(Sentiment ~ ., data = trainTransformeddf, method = "rf", trControl = trainControl(method = "cv"))


# Set up cross-validation
ctrldf <- trainControl(method = "cv", number = 5)


# Make predictions on test data
predictionsdf <- predict(modeldf, testdf)


# Compute confusion matrix and other performance measures
cmdf <- confusionMatrix(predictionsdf, testdf$Sentiment)
cmdf
