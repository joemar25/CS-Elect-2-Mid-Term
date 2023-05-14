# Load necessary packages
library(tidyverse)
library(arules)
library(arulesViz)

# Load Zoo dataset and display first few rows
data(Zoo, package = "mlbench")
head(Zoo)

# Convert Zoo dataset into a transaction object
trans <- transactions(Zoo)

# Create a bar plot to show the distribution of the number of legs among animals
ggplot(Zoo, aes(legs)) + geom_bar()

# Count the number of animals with a certain number of legs and create a logical variable to indicate if each animal has legs
table(Zoo$legs)
Zoo_has_legs <- Zoo %>% mutate(legs = legs > 0)

# Create a bar plot to show the distribution of animals with and without legs
ggplot(Zoo_has_legs, aes(legs)) + geom_bar()

# Count the number of animals with and without legs
table(Zoo_has_legs$legs)

# Convert legs variable into a factor and show unique values of legs
Zoo_unique_leg_values <- Zoo %>% mutate(legs = factor(legs))
head(Zoo_unique_leg_values$legs)

# Discretize legs variable into two intervals and show frequency of animals in each interval
Zoo_discretized_legs <- Zoo %>% mutate(
  legs = discretize(legs, breaks = 2, method="interval")
)
table(Zoo_discretized_legs$legs)

# Convert Zoo_has_legs dataset into a transaction object and display some summary statistics
trans <- transactions(Zoo_has_legs)
summary(trans)
colnames(trans)

# Check the class of variables in the Zoo_has_legs dataset and convert logical variables into factors
sapply(Zoo_has_legs, class)
Zoo_factors <- Zoo_has_legs %>% mutate_if(is.logical, factor)
sapply(Zoo_factors, class)

# Convert Zoo_factors dataset into a transaction object and display item frequency plot
trans_factors <- transactions(Zoo_factors)
itemFrequencyPlot(trans_factors, topN = 20)

# Select transactions that contain a certain item and display them using inspect()
trans_insects <- trans_factors[trans %in% "type=insect"]
trans_insects
inspect(trans_insects)

# Convert transaction object into vertical format and display resulting matrix
vertical <- as(trans, "tidLists")
as(vertical, "matrix")[1:10, 1:5]



# Current learnings...
# in this chapter we have used the Zoo dataset - again.
# We use arules tool to make a "transaction object".
# We also use ggplot2 and arulesViz to see the data. We make use of the mutate(),
# if_else(), and factor() functions, and basic functions like table() and sapply()
# to summarize data and check variable types. And try to make sense of the data
# and find interesting patterns.




# Calculate 2 to the power of the number of columns in the 'trans' object
2^ncol(trans)

# Perform association rule mining on the 'trans' object, specifying the target as "frequent"
its <- apriori(trans, parameter=list(target = "frequent"))

# Calculate the support of the itemset "type=mammal"
5/nrow(trans)

# Perform association rule mining with a minimum support of 5%
its <- apriori(trans, parameter=list(target = "frequent", support = 0.05))

# Sort the rules by support and inspect the top 10
its <- sort(its, by = "support")
inspect(head(its, n = 10))

# Create a bar chart showing the itemset sizes
ggplot(tibble(`Itemset Size` = factor(size(its))), aes(`Itemset Size`)) + geom_bar()

# Inspect the rules with a size greater than 8
inspect(its[size(its) > 8])

# Find the maximal rules using the 'is.maximal()' function
its_max <- its[is.maximal(its)]
its_max
# Inspect the top maximal rules by support
inspect(head(its_max, by = "support"))

# Find the closed rules using the 'is.closed()' function
its_closed <- its[is.closed(its)]
its_closed
# Inspect the top closed rules by support
inspect(head(its_closed, by = "support"))

# Count the number of frequent, closed, and maximal rules
counts <- c(
  frequent=length(its),
  closed=length(its_closed),
  maximal=length(its_max)
)

# Create a bar chart to show the counts of each rule type
ggplot(as_tibble(counts, rownames = "Itemsets"), aes(Itemsets, counts)) + geom_bar(stat = "identity")



# Current learnings...
# the code uses the Apriori algorithm to find patterns in a dataset
# of animal attributes. It calculates support, sorts and inspects the
# top rules, and creates charts to show the counts of each rule type.



# Perform association rule mining on the transaction dataset
rules <- apriori(trans, parameter = list(support = 0.05, confidence = 0.9))

# Get the length of the rules
length(rules)

# Inspect the top rules
inspect(head(rules))

# Get quality measures of the rules
quality(head(rules))

# Sort the rules by lift
rules <- sort(rules, by = "lift")
inspect(head(rules, n = 10))

# Perform Apriori on the transaction dataset with factorized variables
r <- apriori(trans_factors)

# Inspect the rules
inspect(r[1:10])

# Inspect the top rules sorted by lift
inspect(head(r, n = 10, by = "lift"))

# Add interest measures to the rules and inspect the top rules sorted by phi
interestMeasure(rules[1:10], measure = c("phi", "gini"), trans = trans)
quality(rules) <- cbind(quality(rules),
                        interestMeasure(rules, measure = c("phi", "gini"), trans = trans))
inspect(head(rules, by = "phi"))

# Extract the item type labels from the transaction dataset
type <- grep("type=", itemLabels(trans), value = TRUE)

# Use item type labels as the right-hand side of the association rules
rules_type <- apriori(trans, appearance= list(rhs = type))
inspect(head(sort(rules_type, by = "lift")))



# Current learnings...
# the code demonstrates the use of the apriori algorithm to mine frequent itemsets
# and association rules from a dataset of animal characteristics. The code also
# shows how to use interest measures to rank the rules, and how to filter
# rules by appearance.


library(arulesViz)

# plot rules using various methods
plot(rules)
plot(rules, control = list(jitter = 0))
plot(rules, shading = "order")
plot(rules, method = "grouped")
plot(rules, method = "graph")
plot(head(rules, by = "phi", n = 100), method = "graph")

# use iris dataset to create transactions
data(iris)
iris_trans <- transactions(iris)
inspect(head(iris_trans))

# generate rules and inspect using DT package
rules <- apriori(iris_trans, parameter = list(support = 0.1, confidence = 0.8))
rules
inspectDT(rules)

# plot rules using html engine
plot(rules, engine = "html")
plot(rules, method = "matrix", engine = "html") 
plot(rules, method = "graph", engine = "html")



# Current learnings...
# the code is using the arulesViz package to visualize association rules generated
# from transaction data. The first part of the code shows different ways to plot
# the rules, including using different methods such as "graph", "matrix", and
# "grouped". The second part of the code uses the iris dataset to generate
# association rules and visualize them using the arulesViz package. The resulting
# plots can be displayed as interactive HTML files.

# Overall, the code shows how to use arulesViz to visualize and explore
# association rules in a user-friendly way.









# Key takaway
# we use the Apriori algorithm to find patterns in a dataset of animal attributes.
# We calculate support, sort and inspect the top rules, and create charts to show
# the counts of each rule type. We use functions like mutate(), if_else(), and
# factor() to clean and manipulate the data. We also use ggplot2 and arulesViz
# to visualize the data.

