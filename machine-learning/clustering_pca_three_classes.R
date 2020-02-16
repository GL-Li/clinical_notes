# PCA clustering of clinical note in three specialties: Gastroenterology, 
# Neurology, and Urology. Plot with the first two principle components, we
# are able to identify three clusters. As we know the specialty of each 
# sample, we compare the quality of the clustering. 
#
# Results: 
# - Uisng tfidf generated from clinical notes, PCA clustering has 66% 
#   accuracy.
# - Using tfidf generagte from named entities extracted with Amazon Comprehend 
#   Medical from clinical note, PCA clustering has 76% accuracy
# - Both are better than base accuracy of 38% assuming all to be Gastroenterolgy

source("utilities.R")

# Prepare data =================================================================
dat <- read_notes("data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
                  cols_keep = c("id", "amazon_me", "specialty", "note"),
                  clean = TRUE,
                  y_label = TRUE)

# tfidf matrix
tfidf_note <- tfidf_tm(dat$note)
tfidf_amazon <- tfidf_tm(dat$amazon_me)
y_true <- dat$y


# pca using note ===============================================================
# PCA on tfidf generated with clinical notes. Keep 
# only PC1 and PC2 for clustering visualization and analysis

pca <- prcomp(tfidf_note)
pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]

# Plot samples in pc1 - pc2 space. Spot 3 clusters and add line boundary to 
# seperate the clusters
plot(pc1, pc2, col = y + 1)
curve(-3 * x, from = 0, to = 0.3, add = TRUE, col = "red")
curve(5 * x, from = 0, to = 0.3, add = TRUE, col = "green")
curve(0.8 * x, from = 0, to = -0.4, add = TRUE, col = "black")

cluster1 <- (pc2 >= -3 * pc1) & (pc2 < 5 * pc1)
cluster2 <- (pc2 >= 5 * pc1) & (pc2 > 0.8 * pc1)
cluster3 <- (pc2 <= 0.8 * pc1) & (pc2 < -3 * pc1)

# assigne a label to each cluster
y_clusters <- rep(integer(0), length(y_true))
y_clusters[cluster1] <- 1
y_clusters[cluster2] <- 2
y_clusters[cluster3] <- 3

# As we know the true labels y_true, we can use them to determine each cluster
# identified by PCA
table(y_true, y_clusters)
y_pred[y_clusters == 1] <- 1
y_pred[y_clusters == 2] <- 2
y_pred[y_clusters == 3] <- 0

# report metrics
caret::confusionMatrix(as.factor(y_pred), as.factor(y_true))

# pca amazon_me ================================================================
# PCA on tfidf generated with Amazon Comprehend Medical named entities. Keep 
# only PC1 and PC2 for clustering visualization and analysis
pca <- prcomp(tfidf_amazon)
pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]

# Plot samples in pc1 - pc2 space. Spot 3 clusters and add line boundary to 
# seperate the clusters
plot(pc1, pc2, col = y + 1)
curve(-2 * x, from = 0, to = 0.3, add = TRUE, col = "red")
curve(0.8 * x, from = 0, to = 0.6, add = TRUE, col = "green")
curve(-0.8 * x, from = 0, to = -0.4, add = TRUE, col = "black")

cluster1 <- (pc2 >= -2 * pc1) & (pc2 < 0.8 * pc1)
cluster2 <- (pc2 >= 0.8 * pc1) & (pc2 > -0.8 * pc1)
cluster3 <- (pc2 <= -0.8 * pc1) & (pc2 < -2 * pc1)

# assigne a label to each cluster
y_clusters <- rep(integer(0), length(y_true))
y_clusters[cluster1] <- 1
y_clusters[cluster2] <- 2
y_clusters[cluster3] <- 3

# As we know the true labels y_true, we can use them to determine each cluster
# identified by PCA
table(y_true, y_clusters)
y_pred[y_clusters == 1] <- 0
y_pred[y_clusters == 2] <- 2
y_pred[y_clusters == 3] <- 1

# report confusion matrix
caret::confusionMatrix(as.factor(y_pred), as.factor(y_true))


