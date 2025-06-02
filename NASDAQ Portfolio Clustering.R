library(tidyverse)
library(cluster)

# Read Nasdaq data
nasdaq <- read_csv('C:\\Users\\Ashton\\Desktop\\NASDAQ Portfolios - Assignment 1 BUS 336\\NasdaqReturns.csv')

# Select only return columns
nasdaq_returns <- select(nasdaq, -c(1:3))

# Create correlation distance matrix
returns_matrix <- as.matrix(nasdaq_returns)
cor_matrix <- cor(t(returns_matrix))
cor_dist <- as.dist(sqrt(2 * (1 - cor_matrix)))

# Hierarchical clustering with complete method
hc <- hclust(cor_dist, method = "complete")

# function for looping clustering

analyze_clusters <- function(data, cluster_labels, k) {
  data <- data %>% mutate(Cluster = cluster_labels)
  
  cluster_stats <- data.frame(Cluster = integer(), 
                              AnnualizedReturn = numeric(), 
                              StdDev = numeric(), 
                              CV = numeric())
  
  for (i in 1:k) {
    portfolio <- data %>%
      filter(Cluster == i) %>%
      select(starts_with("Ret"))
    
    # Geometric mean return (monthly → annualized)
    row_geo_means <- apply(portfolio, 1, function(x) prod(1 + x)^(1 / length(x)) - 1)
    monthly_geo <- prod(1 + row_geo_means)^(1 / length(row_geo_means)) - 1
    annual_geo <- (1 + monthly_geo)^12 - 1
    
    # Standard deviation (mean across stocks)
    row_stdevs <- apply(portfolio, 1, sd)
    mean_stdev <- mean(row_stdevs) * sqrt(12)  # Annualized standard deviation
    
    # Coefficient of Variation
    cv <- mean_stdev / (annual_geo)
    
    cluster_stats[i, ] <- c(i, annual_geo, mean_stdev, cv)
    
    cat(paste0(
      "Cluster ", i,
      " | Annualized Return: ", round(annual_geo * 100, 4), "%",
      " | CV: ", round(cv, 4), "\n"
    ))
  }
  
  # CV of the whole k-cluster portfolio (averaged)
  overall_cv <- mean(cluster_stats$CV)
  cat(paste0(">>> Mean CV across all ", k, " clusters: ", round(overall_cv, 4), "\n\n"))
}

# Cluster = 2
clusters_2 <- cutree(hc, k = 2)
cat("=== Cluster 2 Analysis ===\n")
analyze_clusters(nasdaq, clusters_2, 2)

# Cluster = 3
clusters_3 <- cutree(hc, k = 3)
cat("\n=== Cluster 3 Analysis ===\n")
analyze_clusters(nasdaq, clusters_3, 3)

# Cluster = 4
clusters_4 <- cutree(hc, k = 4)
cat("\n=== Cluster 4 Analysis ===\n")
analyze_clusters(nasdaq, clusters_4, 4)

# Cluster = 5
clusters_5 <- cutree(hc, k = 5)
cat("\n=== Cluster 5 Analysis ===\n")
analyze_clusters(nasdaq, clusters_5, 5)

# Cluster = 6
clusters_6 <- cutree(hc, k = 6)
cat("\n=== Cluster 6 Analysis ===\n")
analyze_clusters(nasdaq, clusters_6, 6)

# Cluster = 7
clusters_7 <- cutree(hc, k = 7)
cat("\n=== Cluster 7 Analysis ===\n")
analyze_clusters(nasdaq, clusters_7, 7)

# Cluster = 8
clusters_8 <- cutree(hc, k = 8)
cat("\n=== Cluster 8 Analysis ===\n")
analyze_clusters(nasdaq, clusters_8, 8)

# Cluster = 9
clusters_9 <- cutree(hc, k = 9)
cat("\n=== Cluster 9 Analysis ===\n")
analyze_clusters(nasdaq, clusters_9, 9)

# Cluster = 10
clusters_10 <- cutree(hc, k = 10)
cat("\n=== Cluster 10 Analysis ===\n")
analyze_clusters(nasdaq, clusters_10, 10)

# Cluster = 11
clusters_11 <- cutree(hc, k = 11)
cat("\n=== Cluster 11 Analysis ===\n")
analyze_clusters(nasdaq, clusters_11, 11)

# Cluster = 12
clusters_12 <- cutree(hc, k = 12)
cat("\n=== Cluster 12 Analysis ===\n")
analyze_clusters(nasdaq, clusters_12, 12)

# Cluster = 13
clusters_13 <- cutree(hc, k = 13)
cat("\n=== Cluster 13 Analysis ===\n")
analyze_clusters(nasdaq, clusters_13, 13)

# Cluster = 14
clusters_14 <- cutree(hc, k = 14)
cat("\n=== Cluster 14 Analysis ===\n")
analyze_clusters(nasdaq, clusters_14, 14)

# Cluster = 15
clusters_15 <- cutree(hc, k = 15)
cat("\n=== Cluster 15 Analysis ===\n")
analyze_clusters(nasdaq, clusters_15, 15)

#Choose 15, as it is well diversified with a low CV

# plot dendrogram
plot(hc, labels = FALSE, xlab = "Stocks", ylab = "Correlation Distance")
rect.hclust(hc, k = 15, border = "red")

nasdaq_with_clusters <- cbind(nasdaq, Cluster = clusters_15)

write_csv(nasdaq_with_clusters, 'ClusteredNasdaqReturns.csv')


