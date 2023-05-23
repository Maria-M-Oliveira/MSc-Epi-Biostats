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

# .1 Hierarchical cluster analysis
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

# .2 Non-hierarchical clustering,, aka k-means
# For kmean clustering we must first have a predetermined number of clusters, determined before in the hierarchical clustering





