library(tidyverse)
library(cluster)

# Read Nasdaq data
nasdaq <- read_csv('C:\\Users\\Ashton\\Desktop\\NASDAQ Portfolios - Assignment 1 BUS 336\\NasdaqReturns.csv')

# Select only return columns
nasdaq_returns <- select(nasdaq, -c(1:3))

# Create correlation distance matrix
cor_matrix <- cor(t(nasdaq_returns))
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
    
    # Geometric mean return (monthly ??? annualized)
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
      " | CV: ", round(cv, 4), "\n"
    ))
  }
  
  # CV of the whole k-cluster portfolio (averaged)
  overall_sd_cv <- sd(cluster_stats$CV)
  cat(paste0(">>> Standard Deviation of CV across all ", k, " clusters: ", round(overall_sd_cv, 4), "\n\n"))
  
tibble(k, overall_sd_cv)
}


# Cluster = 2
clusters_2 <- cutree(hc, k = 2)
cat("=== Cluster 2 Analysis ===\n")
analyze_clusters(nasdaq, clusters_2, 2)
SD_K_2 <- analyze_clusters(nasdaq, clusters_2, 2)

# Cluster = 3
clusters_3 <- cutree(hc, k = 3)
cat("\n=== Cluster 3 Analysis ===\n")
analyze_clusters(nasdaq, clusters_3, 3)
SD_K_3 <- analyze_clusters(nasdaq, clusters_3, 3)

# Cluster = 4
clusters_4 <- cutree(hc, k = 4)
cat("\n=== Cluster 4 Analysis ===\n")
analyze_clusters(nasdaq, clusters_4, 4)
SD_K_4 <- analyze_clusters(nasdaq, clusters_4, 4)

# Cluster = 5
clusters_5 <- cutree(hc, k = 5)
cat("\n=== Cluster 5 Analysis ===\n")
analyze_clusters(nasdaq, clusters_5, 5)
SD_K_5 <- analyze_clusters(nasdaq, clusters_5, 5)

# Cluster = 6
clusters_6 <- cutree(hc, k = 6)
cat("\n=== Cluster 6 Analysis ===\n")
analyze_clusters(nasdaq, clusters_6, 6)
SD_K_6 <- analyze_clusters(nasdaq, clusters_6, 6)

# Cluster = 7
clusters_7 <- cutree(hc, k = 7)
cat("\n=== Cluster 7 Analysis ===\n")
analyze_clusters(nasdaq, clusters_7, 7)
SD_K_7 <- analyze_clusters(nasdaq, clusters_7, 7)

# Cluster = 8
clusters_8 <- cutree(hc, k = 8)
cat("\n=== Cluster 8 Analysis ===\n")
analyze_clusters(nasdaq, clusters_8, 8)
SD_K_8 <- analyze_clusters(nasdaq, clusters_8, 8)

# Cluster = 9
clusters_9 <- cutree(hc, k = 9)
cat("\n=== Cluster 9 Analysis ===\n")
analyze_clusters(nasdaq, clusters_9, 9)
SD_K_9 <- analyze_clusters(nasdaq, clusters_9, 9)

# Cluster = 10
clusters_10 <- cutree(hc, k = 10)
cat("\n=== Cluster 10 Analysis ===\n")
analyze_clusters(nasdaq, clusters_10, 10)
SD_K_10 <- analyze_clusters(nasdaq, clusters_10, 10)

# Cluster = 11
clusters_11 <- cutree(hc, k = 11)
cat("\n=== Cluster 11 Analysis ===\n")
analyze_clusters(nasdaq, clusters_11, 11)
SD_K_11 <- analyze_clusters(nasdaq, clusters_11, 11)

# Cluster = 12
clusters_12 <- cutree(hc, k = 12)
cat("\n=== Cluster 12 Analysis ===\n")
analyze_clusters(nasdaq, clusters_12, 12)
SD_K_12 <- analyze_clusters(nasdaq, clusters_12, 12)

# Cluster = 13
clusters_13 <- cutree(hc, k = 13)
cat("\n=== Cluster 13 Analysis ===\n")
analyze_clusters(nasdaq, clusters_13, 13)
SD_K_13 <- analyze_clusters(nasdaq, clusters_13, 13)

# Cluster = 14
clusters_14 <- cutree(hc, k = 14)
cat("\n=== Cluster 14 Analysis ===\n")
analyze_clusters(nasdaq, clusters_14, 14)
SD_K_14 <- analyze_clusters(nasdaq, clusters_14, 14)

# Cluster = 15
clusters_15 <- cutree(hc, k = 15)
cat("\n=== Cluster 15 Analysis ===\n")
analyze_clusters(nasdaq, clusters_15, 15)
SD_K_15 <- analyze_clusters(nasdaq, clusters_15, 15)

# Cluster = 16
clusters_16 <- cutree(hc, k = 16)
cat("\n=== Cluster 16 Analysis ===\n")
analyze_clusters(nasdaq, clusters_16, 16)
SD_K_16 <- analyze_clusters(nasdaq, clusters_16, 16)

# Cluster = 17
clusters_17 <- cutree(hc, k = 17)
cat("\n=== Cluster 17 Analysis ===\n")
analyze_clusters(nasdaq, clusters_17, 17)
SD_K_17 <- analyze_clusters(nasdaq, clusters_17, 17)

# Cluster = 18
clusters_18 <- cutree(hc, k = 18)
cat("\n=== Cluster 18 Analysis ===\n")
analyze_clusters(nasdaq, clusters_18, 18)
SD_K_18 <- analyze_clusters(nasdaq, clusters_18, 18)

# Cluster = 19
clusters_19 <- cutree(hc, k = 19)
cat("\n=== Cluster 19 Analysis ===\n")
analyze_clusters(nasdaq, clusters_19, 19)
SD_K_19 <- analyze_clusters(nasdaq, clusters_19, 19)

# Cluster = 20
clusters_20 <- cutree(hc, k = 20)
cat("\n=== Cluster 20 Analysis ===\n")
analyze_clusters(nasdaq, clusters_20, 20)
SD_K_20 <- analyze_clusters(nasdaq, clusters_20, 20)

# Cluster = 21
clusters_21 <- cutree(hc, k = 21)
cat("\n=== Cluster 21 Analysis ===\n")
analyze_clusters(nasdaq, clusters_21, 21)
SD_K_21 <- analyze_clusters(nasdaq, clusters_21, 21)

# Cluster = 22
clusters_22 <- cutree(hc, k = 22)
cat("\n=== Cluster 22 Analysis ===\n")
analyze_clusters(nasdaq, clusters_22, 22)
SD_K_22 <- analyze_clusters(nasdaq, clusters_22, 22)

# Cluster = 23
clusters_23 <- cutree(hc, k = 23)
cat("\n=== Cluster 23 Analysis ===\n")
analyze_clusters(nasdaq, clusters_23, 23)
SD_K_23 <- analyze_clusters(nasdaq, clusters_23, 23)

all_SD <- rbind(
  SD_K_2,
  SD_K_3,
  SD_K_4,
  SD_K_5,
  SD_K_6,
  SD_K_7,
  SD_K_8,
  SD_K_9,
  SD_K_10,
  SD_K_11,
  SD_K_12,
  SD_K_13,
  SD_K_14,
  SD_K_15,
  SD_K_16,
  SD_K_17,
  SD_K_18,
  SD_K_19,
  SD_K_20,
  SD_K_21,
  SD_K_22,
  SD_K_23
)


ggplot(data = all_SD, aes(x = k, y = overall_sd_cv)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 31.51660, linetype = "dashed") +
  geom_text( x = 3,        
             y = 31.51660,                 
             label = "31.52",       
             hjust = 1.1,                  
             vjust = -0.5,                 
             size = 3.5) +
  scale_x_continuous(n.breaks = 23) +
  labs(x = "Cluster Size",
       y = "Standard Deviation of the CV of Returns",
       title = "Standard Deviation of the CV of each\nClusters Monthly Returns for Varying Cluster Sizes") +
  theme(plot.title = element_text(hjust = 0.5))
  
#Choose 18, as it has the lowest SD(CV)

# plot dendrogram
plot(hc, labels = FALSE, xlab = "Stocks", ylab = "Correlation Distance")
rect.hclust(hc, k = 18, border = "red")

nasdaq_with_clusters <- cbind(nasdaq, Cluster = clusters_18)

write_csv(nasdaq_with_clusters, 'ClusteredNasdaqReturns.csv')



     