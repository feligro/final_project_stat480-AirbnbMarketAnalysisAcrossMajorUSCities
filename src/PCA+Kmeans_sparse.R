set.seed(4801)
all_listings<- readRDS("all_listings.rds")
amen <- readRDS("amenities_sparse.rds")
              # ---- AMENITIES TRANSFORMATION ----
library(irlba)

# Number of components to extract - 10–30 is typical
k <- 20

# Sparse PCA via truncated SVD
set.seed(4801)
amen_pca <- irlba(amen, nv = k, center = TRUE, scale = FALSE)

amen_scores <- amen %*% amen_pca$v

# Approximate explained variance
var_explained <- amen_pca$d^2 / sum(amen_pca$d^2)
var_explained  # check how much each PC contributes

  # We just came to realize that with 6PC's we explain 2/3 of the variation. Let's try with it.
sum(var_explained[1:6])

amen_pca <- irlba(amen, nv = 6, center = TRUE, scale = FALSE)
# ---- SIGN FIXING ----
rownames(amen_pca$v) <- colnames(amen)
loadings <- amen_pca$v

ref_vars_amen <- c("wifi","heating","self check-in","bbq grill",
                   "kitchen","fire extinguisher")

for(j in seq_along(ref_vars_amen)){
  v <- ref_vars_amen[j]
  if(v %in% rownames(loadings)){
    if(loadings[v, j] < 0) loadings[, j] <- -loadings[, j]
  }
}

amen_pca$v <- loadings
amen_scores <- amen %*% amen_pca$v

rownames(amen_pca$v) <- colnames(amen)
loadings <- amen_pca$v
top_loadings <- function(component, n = 5) {
  x <- loadings[, component]
  idx <- order(x, decreasing = TRUE)
  list(
    top_positive = names(x[idx[1:n]]),
    top_negative = names(x[idx[(length(x)-n+1):length(x)]])
  )
}
top_loadings(1)   # Basic Interior Setup vs. Modern Comfort Amenities
top_loadings(2)   # Heating Indoor & Essentials vs. Outdoor/Shared Features
top_loadings(3)   # Urban Self-Service Units vs. Home-Style Amenities
top_loadings(4)   # Outdoor & Pet-Friendly Living vs. Indoor Personal-Care Focus
top_loadings(5)   # Full Kitchen Functionality vs. Safety & Host Essentials
top_loadings(6)   # Practical/Security Amenities vs. Luxury Building Features

loadings <- amen_pca$v      # PCA loadings
rownames(loadings) <- colnames(amen)

#visualizing PC's
plot_top_loadings <- function(pc, n = 10) {
  x <- loadings[, pc]
  idx <- order(abs(x), decreasing = TRUE)[1:n]
  barplot(
    x[idx],
    las = 2,
    main = paste("Top", n, "Loadings for PC", pc),
    col = ifelse(x[idx] > 0, "steelblue", "tomato"),
    ylab = "Loading value"
  )
}
par(mfrow=c(2,3))
for (i in 1:6) plot_top_loadings(i)
par(mfrow=c(1,1))


    # CHECKING INTERPRETABILITY
set.seed(123)
idx <- sample(1:nrow(amen), 3)
idx
get_amenities <- function(i){
  idx <- which(amen[i, ] == 1)
  colnames(amen)[idx]
}
amenities_1 <- get_amenities(idx[1])
amenities_2 <- get_amenities(idx[2])
amenities_3 <- get_amenities(idx[3])
amen_scores <- amen %*% amen_pca$v
pca1 <- amen_scores[idx[1], ]
pca2 <- amen_scores[idx[2], ]
pca3 <- amen_scores[idx[3], ]
amenities_1
pca1
  # PC1 high; comfort amenities. PC4 slightly high; outdoor features. PC5 low; safety items included
  # Outdoor-friendly, suburban, family-ready home
amenities_2
pca2
  # PC1 very high; luxury & complete. PC3 slightly high; self-check-in/urban. PC4 neutral; not strongly outdoor
  # High-comfort, smart-tech, fully equipped suburban home
amenities_3
pca3
  # PC1 medium; modern comfort. PC2 high; strong indoor essentials. PC5 very low; no kitchen. PC6 low; building-level amenities
  # Urban minimalist / hotel-style unit

    # K-means part
set.seed(123)

results <- lapply(3:20, function(k){
  km <- kmeans(amen_scores, centers = k, nstart = 50, iter.max = 1000)
  km$tot.withinss
})
#elbow plot
plot(3:20, unlist(results), type="b", xlab="K", ylab="Total within-cluster SSE", main = "Elbow Plot for K Selection")

library(cluster)
#silhouette score
set.seed(123)

# Subsample rows
idx <- sample(1:nrow(amen_scores), 3000)
sub_scores <- amen_scores[idx, ]

sil <- sapply(3:20, function(k){
  km <- kmeans(sub_scores, centers=k, nstart=50, iter.max = 1000)
  d <- dist(sub_scores)
  ss <- silhouette(km$cluster, d)
  mean(ss[,3])
})

plot(3:20, sil, type="b", xlab="K", ylab="Silhouette score", main = "Silhouette Analysis for K")

  # We choose K=8. None of the plots gave any satsifactory criteria
# for determining a K, and this is the best approximation.
K <- 8

set.seed(123)
kfit <- kmeans(amen_scores, centers = K, nstart = 50, iter.max = 1000)

cluster_labels <- kfit$cluster

amen_scores_mat <- as.matrix(amen_scores)
cluster_pca_means <- aggregate(amen_scores_mat, 
                               by = list(cluster = cluster_labels), 
                               FUN = mean)
cluster_pca_means
all_listings$amen_cluster <- cluster_labels #adding new variable

    # visualization
library(reshape2)
library(ggplot2)

df_long <- melt(cluster_pca_means, id.vars="cluster")

ggplot(df_long, aes(x = variable, y = factor(cluster), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                       midpoint=0) +
  theme_minimal() +
  labs(title="Cluster PCA Score Heatmap",
       x="Principal Component",
       y="Cluster")



              # ---- QUESTION 3 ----
set.seed(4801)
library(fastDummies)
library(dplyr)
  # STEP1: Choosing and transforming variables
df <- all_listings %>%
  select(
    log_price,
    occupancy_rate,
    number_of_reviews,
    reviews_per_month,
    review_scores_rating,
    amenity_count,
    accommodates,
    bedrooms,
    beds,
    bathrooms,
    host_is_superhost,
    host_listings_count,
    host_identity_verified,
    availability_365
  )

df <- scale(df)

  # PCA
pca_list <- prcomp(df, center = TRUE, scale. = FALSE)
# ---- SIGN FIXING FOR LISTING PCA ----
fix_signs <- function(pca_obj, ref_vars){
  L <- pca_obj$rotation
  S <- pca_obj$x
  
  for (j in seq_along(ref_vars)) {
    v <- ref_vars[j]
    if (v %in% rownames(L)) {
      if (L[v, j] < 0) {
        L[, j] <- -L[, j]
        S[, j] <- -S[, j]
      }
    }
  }
  
  pca_obj$rotation <- L
  pca_obj$x <- S
  return(pca_obj)
}

ref_vars <- c("log_price",
              "number_of_reviews",
              "host_is_superhost",
              "occupancy_rate",
              "review_scores_rating",
              "availability_365")

pca_list <- fix_signs(pca_list, ref_vars)

summary(pca_list)

plot_top_loadings <- function(pca_obj, pc = 1, n = 10) {
  loadings <- pca_obj$rotation[, pc]
  idx <- order(abs(loadings), decreasing = TRUE)[1:n]
  
  barplot(loadings[idx],
          las = 2,
          col = ifelse(loadings[idx] > 0, "steelblue", "tomato"),
          main = paste("Top", n, "Loadings for PC", pc),
          ylab = "Loading")
}
par(mfrow=c(2,3))
for (i in 1:6) plot_top_loadings(pca_list, pc = i, n = 10)
par(mfrow=c(1,1))

  # Let's choose again PC6 (almost 80% of variance is explained)
scores_list <- pca_list$x[, 1:6]

    #K_MEANS
#elbow
set.seed(123)
k_range <- 3:12

wcss <- sapply(k_range, function(k) {
  kmeans(scores_list, centers = k, nstart = 50, iter.max = 1000)$tot.withinss
})

plot(k_range, wcss, type="b", pch=19,
     xlab = "Number of clusters K",
     ylab = "Total within-cluster SSE",
     main = "Elbow Plot for K Selection")
#silhouette
idx_sub <- sample(1:nrow(scores_list), 4000)
scores_sub <- scores_list[idx_sub, ]

sil_vals <- sapply(3:12, function(k){
  km <- kmeans(scores_sub, centers = k, nstart = 50, iter.max = 1000)
  ss <- silhouette(km$cluster, dist(scores_sub))
  mean(ss[, 3])
})

plot(3:12, sil_vals, type="b", pch=19,
     xlab = "K",
     ylab = "Average silhouette width",
     main = "Silhouette Analysis for K")

  #We choose k=5. Silhouette clearly peaks at K = 5
#and Elbow stabilizes at K = 5
set.seed(123)
km_main <- kmeans(scores_list, centers = 6, nstart = 50, iter.max = 1000)
all_listings$main_cluster <- km_main$cluster

cluster_summary_main <- all_listings %>%
  group_by(main_cluster) %>%
  summarise(
    n = n(),
    avg_price = mean(price, na.rm=TRUE),
    avg_log_price = mean(log_price, na.rm=TRUE),
    avg_occupancy = mean(occupancy_rate, na.rm=TRUE),
    avg_accommodates = mean(accommodates, na.rm=TRUE),
    avg_beds = mean(beds, na.rm=TRUE),
    avg_bedrooms = mean(bedrooms, na.rm=TRUE),
    avg_bathrooms = mean(bathrooms, na.rm=TRUE),
    avg_amenity_count = mean(amenity_count, na.rm=TRUE),
    avg_reviews = mean(number_of_reviews, na.rm=TRUE),
    avg_review_score = mean(review_scores_rating, na.rm=TRUE),
    prop_superhost = mean(host_is_superhost, na.rm=TRUE),
    avg_host_listings = mean(host_listings_count, na.rm=TRUE)
  )
cluster_summary_main

cluster_pca_main <- aggregate(scores_list,
                              by=list(cluster = km_main$cluster),
                              FUN=mean)

cluster_pca_main


df_long <- melt(cluster_pca_main, id.vars = "cluster")

ggplot(df_long, aes(x = variable, y = factor(cluster), fill = value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                       midpoint=0) +
  theme_minimal() +
  labs(title="Cluster PCA Score Heatmap",
       x="Principal Component",
       y="Cluster")
