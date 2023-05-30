### Libraries
library(tidyverse)
library(foreign)
library(factoextra)
library(cluster)

### Files
BD <- read.spss("./MSc-Epi-Biostats/BaseCovid.sav", to.data.frame = TRUE)

#subsetting bc i only want a few variables and standardizing
df <- BD %>% 
  select(c(country,total_cases_per_million_10, total_deaths_per_million_10, new_cases_per_million_10, new_deaths_per_million_10)) %>% 
  # next line to standardize our numeric variables
  mutate_at(c("total_cases_per_million_10", "total_deaths_per_million_10", "new_cases_per_million_10", "new_deaths_per_million_10"), ~(scale(.) %>% as.vector)) %>%
  # assigning the country column to rownames for the next steps (factoextra package)
  column_to_rownames("country")

### Exercise 1
# Apply hierarchical and non hierarchical cluster analysis to the df and select the most adequate number of clusters
# Clustering is a technique in machine learning that attempts to find groups or clusters of observations within a dataset such that the observations within each cluster are quite similar to each other, while observations in different clusters are quite different from each other.

#### .1 Hierarchical cluster analysis  ####
# Proximity measure: Euclidean distance, Manhattan distance, Pearson correlation, etc.
# Aggregation criteria: Complete linkage, Single linkage, Ward's minimum variance, etc.

# 1st Calculate the pairwise dissimilarity/correlation between each observation
dist.cor <- get_dist(df, method = "pearson") #proximity measure: Pearson correlation bc continuous variable
fviz_dist(dist.cor)

# 2nd Set your aggregation criteria and start clustering
clust <- hclust(dist.cor, method = "single") #single linkage in this case
plot(clust)

gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#cut the dendrogram into x clusters
fit <- cutree(clust, k = 5 )
fit

#find number of observations in each cluster
table(fit)

# Add cluster rectangles to dendogram
rect.hclust(clust, k = 5, border = "green")

#### .2 Non-hierarchical clustering,, aka k-means ####
# For k-means clustering we must first have a predetermined number of clusters, determined before in the hierarchical clustering

fviz_nbclust(df, kmeans, method = "wss")
# In the previous graph we are looking for the "elbow" point, indicative of the optimal number of clusters
# It seems there is a bit of a bend around 2, but other options could be considered

# Performing k means analysis
km <- kmeans(df, centers = 2, nstart = 25)
km
# 1st clusters has n=30 and 2nd cluster has n=177

#plot results of final k-means model
fviz_cluster(km, data = df)

#find means of each cluster
aggregate(df, by=list(cluster=km$cluster), mean)

#add cluster assignment to original data
final_data <- cbind(df, cluster = km$cluster)

#view final data
head(final_data)


### Exercise 3
#### Principal components analysis ####
#calculate principal components
#  scale = TRUE so that each of the variables in the dataset are scaled to have a mean of 0 and a std of 1 before calculating PC
results <- prcomp(df, scale = TRUE)

# eigenvectors in R point in the negative direction by default, so we’ll multiply by -1 to reverse the signs
#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

# PC results for each country in this case (would be for each index number/variable)
#reverse the signs of the scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

# Visualizing 
# first and second principal components as the axes
# scale = 0 ensures that the arrows in the plot are scaled to represent the loadings.
biplot(results, scale = 0)

fviz_pca_biplot(results,
                label="var")

# countries close to each other on the plot have similar data patterns in regards to the variables in the original dataset

# Checking if what we see graphically is true in the results
# in the plot we see Bahamas as the country closest to the new_deaths_per_million_10 variable
# This is confirmed by the data, as we see Bahamas at the top of the df
head(df[order(-df$new_deaths_per_million_10),])

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)
var_explained
# 0.61071725 0.17727210 0.12354424 0.08846641
# The 1st component explains 61% total variance in the dataset
# The second principal component explains 17,7% of the total variance in the dataset.
# The third principal component explains 12,4% of the total variance in the dataset.
# The fourth principal component explains 8,8% of the total variance in the dataset.


#create scree plot
# plot that displays the total variance explained by each principal component – to visualize the results of PCA
qplot(c(1:4), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Another way to represent the scree plot
fviz_eig(results, 
         addlabels = TRUE, 
         ylim = c(0, 70))

# As such, the first two principal components explain a majority of the total variance in the data and would be the selected components for interpretation
