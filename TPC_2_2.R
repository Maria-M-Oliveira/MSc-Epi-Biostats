### Libraries
library(tidyverse)
library(foreign)
library(factoextra)

### Files
BD <- read.spss("BaseCovid.sav", to.data.frame = TRUE)

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
clust <- hclust(dist.cor, method = "single")
plot(clust)

# 2nd Fuse observations into clusters


