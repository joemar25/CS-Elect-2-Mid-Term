# Cardiño, Joemar J.
# BSCS 3C
# Chapter 7 - Clustering Analysis

library(tidyverse)

pkgs <- sort(c('tidyverse', 'factoextra', 'dbscan', 'cluster', 'mclust', 
               'kernlab', 'e1071', 'scatterpie', 'fpc', 'seriation', 'mlbench', 'GGally'
))

pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)) install.packages(pkgs_install)

## Data Preparation

data(ruspini, package = "cluster")
# shuffle the data with sample_frac
ruspini <- as_tibble(ruspini) %>% sample_frac()
ruspini

## Data Cleaning
ggplot(ruspini, aes(x = x, y = y)) + geom_point()
summary(ruspini)

## scale data
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

ruspini_scaled <- ruspini %>% scale_numeric()
summary(ruspini_scaled)



# Clustering Methods

## Perform k-means clustering with 4 centers and 10 starting points
km <- kmeans(ruspini_scaled, centers = 4, nstart = 10)
km

## Add cluster column to original data using the k-means cluster assignments
ruspini_clustered <- ruspini_scaled %>% add_column(cluster = factor(km$cluster))

## Print data with added cluster column
ruspini_clustered

## Visualize k-means clustering results with ggplot2
ggplot(ruspini_clustered, aes(x = x, y = y, color = cluster)) + geom_point()

## Extract cluster centroids and store them in a tibble
centroids <- as_tibble(km$centers, rownames = "cluster")

## Print cluster centroids
centroids

## Visualize k-means clustering results with centroids using ggplot2
ggplot(ruspini_clustered, aes(x = x, y = y, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x = x, y = y, color = cluster), shape = 3, size = 10)

## Visualize k-means clustering results with factoextra package
library(factoextra)
fviz_cluster(km, data = ruspini_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

## Visualize cluster profiles with ggplot2 and pivot_longer function
ggplot(pivot_longer(centroids, cols = c(x, y), names_to = "feature"),
       aes(x = value, y = feature, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

## Extract and visualize a single cluster
cluster1 <- ruspini_clustered %>% filter(cluster == 1)
cluster1
summary(cluster1)
ggplot(cluster1, aes(x = x, y = y)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

## Perform k-means clustering with 8 centers and visualize results
fviz_cluster(kmeans(ruspini_scaled, centers = 8), data = ruspini_scaled,
             centroids = TRUE, geom = "point", ellipse.type = "norm")

## Perform hierarchical clustering with complete linkage and visualize dendrogram
d <- dist(ruspini_scaled)
hc <- hclust(d, method = "complete")
plot(hc)

## Visualize hierarchical clustering results with factoextra package
fviz_dend(hc, k = 4)

## Add cluster column to original data using hierarchical clustering results
clusters <- cutree(hc, k = 4)
cluster_complete <- ruspini_scaled %>%
  add_column(cluster = factor(clusters))
cluster_complete

## Visualize hierarchical clustering results with ggplot2
ggplot(cluster_complete, aes(x, y, color = cluster)) +
  geom_point()

## Visualize hierarchical clustering results with 8 clusters
fviz_cluster(list(data = ruspini_scaled, cluster = cutree(hc, k = 8)), geom = "point")

## Perform hierarchical clustering with single linkage and visualize dendrogram
hc_single <- hclust(d, method = "single")
fviz_dend(hc_single, k = 4)

## Visualize hierarchical clustering results with single linkage
fviz_cluster(list(data = ruspini_scaled, cluster = cutree(hc_single, k = 4)), geom = "point")

## Perform density-based clustering with DBSCAN and visualize results
library(dbscan)
kNNdistplot(ruspini_scaled, k = 3)
abline(h = .32, col = "red")

# dbscan
db <- dbscan(ruspini_scaled, eps = .32, minPts = 4)
db
str(db)
ggplot(ruspini_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x, y, color = cluster)) + geom_point()
fviz_cluster(db, ruspini_scaled, geom = "point")

##Partitioning Around Medoids (PAM)
library(cluster)
d <- dist(ruspini_scaled)
str(d)
p <- pam(d, k = 4)
p
ruspini_clustered <- ruspini_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(ruspini_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(ruspini_clustered, aes(x = x, y = y, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = x, y = y, color = cluster), shape = 3, size = 10)
## __Note:__ `fviz_cluster` needs the original data.
fviz_cluster(c(p, list(data = ruspini_scaled)), geom = "point", ellipse.type = "norm")

## Gaussian Mixture Models
library(mclust)
m <- Mclust(ruspini_scaled)
summary(m)
plot(m, what = "classification")
# rerun with a fixed number of 4 clusters
m <- Mclust(ruspini_scaled, G=4)
summary(m)
plot(m, what = "classification")

## Spectral Clustering
library("kernlab")
cluster_spec <- specc(as.matrix(ruspini_scaled), centers = 4)
cluster_spec
ggplot(ruspini_scaled %>% add_column(cluster = factor(cluster_spec)),
       aes(x, y, color = cluster)) + geom_point()

## uzzy C-Means Clustering
library("e1071")

cluster_cmeans <- cmeans(as.matrix(ruspini_scaled), centers = 4)
cluster_cmeans
library("scatterpie")
ggplot()  +
  geom_scatterpie(data = cbind(ruspini_scaled, cluster_cmeans$membership),
                  aes(x = x, y = y), cols = colnames(cluster_cmeans$membership), legend_name = "Membership") + coord_equal()

# the code performs various clustering analyses on the "ruspini" dataset.
# It uses different clustering algorithms, including k-means clustering with different
# numbers of centers, hierarchical clustering with complete and single linkage,
# density-based clustering with DBSCAN, partitioning around medoids (PAM),
# Gaussian mixture models (GMM), spectral clustering, and fuzzy c-means clustering.
# The results of each analysis are visualized using ggplot2 and factoextra packages.
# The code also adds a cluster column to the original data using the cluster
# assignments obtained from each clustering analysis and extracts and prints the
# cluster centroids.



# Internal Cluster Validation 

## Compare the clustering quality using cluster.stats() function
fpc::cluster.stats(d, km$cluster)

## Compare the quality of clusters produced by k-means, hierarchical clustering with complete linkage, and hierarchical clustering with single linkage methods
sapply(list(km = km$cluster, hc_compl = cutree(hc, k = 4), hc_single = cutree(hc_single, k = 4)),
       FUN = function(x) fpc::cluster.stats(d, x))[c("within.cluster.ss", "avg.silwidth"), ]

## Plot silhouette plot
plot(silhouette(km$cluster, d))
fviz_silhouette(silhouette(km$cluster, d))

## Find optimal number of cluster for k-means using elbow method and average silhouette width
ggplot(ruspini_scaled, aes(x, y)) + geom_point()
set.seed(1234)
ks <- 2:10

## Elbow Method: Within-Cluster Sum of Squares
WCSS <- sapply(ks, FUN = function(k) {
  kmeans(ruspini_scaled, centers = k, nstart = 5)$tot.withinss
})
ggplot(tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() + geom_vline(xintercept = 4, color = "red", linetype = 2)

## Average silhouette width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(ruspini_scaled, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k <- ks[which.max(ASW)]
ggplot(tibble(ks, ASW), aes(ks, ASW)) + geom_line() + geom_vline(xintercept = best_k, color = "red", linetype = 2)

## Dunn Index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(ruspini_scaled, centers=k, nstart=5)$cluster)$dunn
})
best_k <- ks[which.max(DI)]
ggplot(tibble(ks, DI), aes(ks, DI)) + geom_line() + geom_vline(xintercept = best_k, color = "red", linetype = 2)

## Gap Statistic
k <- clusGap(ruspini_scaled, FUN = kmeans, nstart = 10, K.max = 10)
plot(k)

## Visualize the clusters with different methods
ggplot(ruspini_scaled, aes(x, y, color = factor(km$cluster))) + geom_point()
d <- dist(ruspini_scaled)
pimage(d, col = bluered(100))
pimage(d, order=order(km$cluster), col = bluered(100))
dissplot(d, labels = km$cluster, options=list(main="k-means with k=4"))
dissplot(d, labels = kmeans(ruspini_scaled, centers = 3)$cluster, col = bluered(100))
dissplot(d, labels = kmeans(ruspini_scaled, centers = 9)$cluster, col = bluered(100))

## Visualize distance matrix using factoextra
fviz_dist(d)

# The code performs various clustering and visualization techniques to analyze the quality
# of the clusters formed using the k-means and hierarchical clustering algorithms.
# It also helps to determine the optimal number of clusters for the k-means algorithm by
# using different techniques such as the elbow method, average silhouette width, Dunn Index,
# and Gap Statistic. Finally, the code visualizes the clusters and distance matrix using
# different methods such as silhouette plot, dissimilarity plot, and factoextra.



#### External Cluster Validation


library(mlbench)
set.seed(1234)
shapes <- mlbench.smiley(n = 500, sd1 = 0.1, sd2 = 0.05)
plot(shapes)
# Prepare data
truth <- as.integer(shapes$class)
shapes <- scale(shapes$x)
colnames(shapes) <- c("x", "y")
shapes <- as_tibble(shapes)

ggplot(shapes, aes(x, y)) + geom_point()

# Find optimal number of clusters for k-means
ks <- 2:20
# use for the sum of squares (look for the knee)
WCSS <- sapply(ks, FUN = function(k) {
  kmeans(shapes, centers = k, nstart = 10)$tot.withinss
})

ggplot(tibble(ks, WCSS), aes(ks, WCSS)) + geom_line()

# looks like ot could be 7 clusters
km <- kmeans(shapes, centers = 7, nstart = 10)

ggplot(shapes %>% add_column(cluster = factor(km$cluster)), aes(x, y, color = cluster)) +
  geom_point()

#Hierarchical clustering
d <- dist(shapes)
hc <- hclust(d, method = "single")
# find optimal number of clusters
ASW <- sapply(ks, FUN = function(k) {
  fpc::cluster.stats(d, cutree(hc, k))$avg.silwidth
})

ggplot(tibble(ks, ASW), aes(ks, ASW)) + geom_line()

# from the plot, maximum is clearly at 4 clusters
hc_4 <- cutree(hc, 4)

ggplot(shapes %>% add_column(cluster = factor(hc_4)), aes(x, y, color = cluster)) +
  geom_point()

# Compare with ground truth with the corrected (=adjusted) Rand index (ARI), 
#   the variation of information (VI) index, entropy and purity.

entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  
  sum(w * rowSums(e, na.rm = TRUE))
}

purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(w * apply(p, 1, max))
}

# calculate measures (for comparison we also use random “clusterings” with 4 and 6 clusters)
random_4 <- sample(1:4, nrow(shapes), replace = TRUE)
random_6 <- sample(1:6, nrow(shapes), replace = TRUE)

r <- rbind(
  kmeans_7 = c(
    unlist(fpc::cluster.stats(d, km$cluster, truth, compareonly = TRUE)),
    entropy = entropy(km$cluster, truth),
    purity = purity(km$cluster, truth)
  ),
  hc_4 = c(
    unlist(fpc::cluster.stats(d, hc_4, truth, compareonly = TRUE)),
    entropy = entropy(hc_4, truth),
    purity = purity(hc_4, truth)
  ),
  random_4 = c(
    unlist(fpc::cluster.stats(d, random_4, truth, compareonly = TRUE)),
    entropy = entropy(random_4, truth),
    purity = purity(random_4, truth)
  ),
  random_6 = c(
    unlist(fpc::cluster.stats(d, random_6, truth, compareonly = TRUE)),
    entropy = entropy(random_6, truth),
    purity = purity(random_6, truth)
  )
)
r



# Advanced Data Preparation for Clustering

library(dbscan)

# Add a clear outlier to the scaled Ruspini dataset that is 10 standard deviations above the average for the x axis
ruspini_scaled_outlier <- ruspini_scaled %>% add_case(x=10,y=0)

# Visual inspection of the data
library("GGally")
ggpairs(ruspini_scaled_outlier, progress = FALSE)

#outlier is a problem for k-means
km <- kmeans(ruspini_scaled_outlier, centers = 4, nstart = 10)
ruspini_scaled_outlier_km <- ruspini_scaled_outlier%>%
  add_column(cluster = factor(km$cluster))
centroids <- as_tibble(km$centers, rownames = "cluster")

ggplot(ruspini_scaled_outlier_km, aes(x = x, y = y, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x = x, y = y, color = cluster), shape = 3, size = 10)


# Local Outlier Factor (LOF)
lof <- lof(ruspini_scaled_outlier, minPts= 10)
lof
ggplot(ruspini_scaled_outlier %>% add_column(lof = lof), aes(x, y, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")

# Plot the points sorted by increasing LOF and look for a knee
ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = 2)

# choose threshold above 1
ggplot(ruspini_scaled_outlier %>% add_column(outlier = lof >= 2), aes(x, y, color = outlier)) +
  geom_point()

# Analyze the found outliers, then cluster data without them
ruspini_scaled_clean <- ruspini_scaled_outlier  %>% filter(lof < 2)

km <- kmeans(ruspini_scaled_clean, centers = 4, nstart = 10)
ruspini_scaled_clean_km <- ruspini_scaled_clean%>%
  add_column(cluster = factor(km$cluster))
centroids <- as_tibble(km$centers, rownames = "cluster")

ggplot(ruspini_scaled_clean_km, aes(x = x, y = y, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x = x, y = y, color = cluster), shape = 3, size = 10)

## Clustering tendency

## use smiley data
library(mlbench)
shapes <- mlbench.smiley(n = 500, sd1 = 0.1, sd2 = 0.05)$x
colnames(shapes) <- c("x", "y")
shapes <- as_tibble(shapes)

## scatter plots
ggplot(shapes, aes(x = x, y = y)) + geom_point()
library(seriation)

d_shapes <- dist(scale(shapes))
VAT(d_shapes, col = bluered(100))

## iVAT - uses the largest distances for all possible paths between two objects
iVAT(d_shapes, col = bluered(100))

## Hopkins statistic
get_clust_tendency(shapes, n = 10)

## Data Without Clustering Tendency
data_random <- tibble(x = runif(500), y = runif(500))
ggplot(data_random, aes(x, y)) + geom_point()

## No point clouds visible, just noise
d_random <- dist(data_random)
VAT(d_random, col = bluered(100))
iVAT(d_random, col = bluered(100))
get_clust_tendency(data_random, n = 10, graph = FALSE)

## K-means on data without clustering tendency
km <- kmeans(data_random, centers = 4)
random_clustered<- data_random %>% add_column(cluster = factor(km$cluster))
ggplot(random_clustered, aes(x = x, y = y, color = cluster)) + geom_point()



# In this code, I learned how to perform clustering analysis on a dataset using various
# clustering techniques in R. First, we prepared and cleaned the data, and then scaled
# it for better analysis. Then used the k-means clustering method with 4 centers to
# cluster the data and visualized the results using ggplot2 and the factoextra package.
# I also learned and observed how to perform hierarchical clustering with complete and single
# linkage methods, and how to visualize the results using dendrograms and the
# fviz_cluster function. Additionally, We have used the DBSCAN clustering method
# to identify clusters with varying densities in the dataset.
# Finally, I learned about the Partitioning Around Medoids (PAM)
# clustering method and used it to identify clusters in the dataset.
# Overall, the source code provided me with a comprehensive understanding of different
# clustering techniques and how to apply them in R.
