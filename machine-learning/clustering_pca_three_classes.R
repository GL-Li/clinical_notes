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

# # Prepare data =================================================================
# dat_gas_neu_uro <- read_notes("data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
#                   cols_keep = c("id", "amazon_me", "specialty", "note"),
#                   clean = TRUE,
#                   y_label = TRUE)
# 
# # tfidf matrix, as sample order is randomized, so save all of them together with
# # pcas
# tfidf_note <- tfidf_tm(dat_gas_neu_uro$note)
# tfidf_amazon <- tfidf_tm(dat_gas_neu_uro$amazon_me)
# y_true <- dat_gas_neu_uro$y
# 
# # the signs of pc1 and pc2 change randomly. For reproducibility in plot, save
# # them as RData
# pca_note <- prcomp(tfidf_note)
# pca_amazon <- prcomp(tfidf_amazon)
# save(dat_gas_neu_uro, tfidf_note, tfidf_amazon, y_true, pca_note, pca_amazon, 
#      file = "shiny-apps/RData/pca_note_amazon_gas_neu_uro.RData")

# load the same data for pca, kmeans, and hcluster
load("shiny-apps/RData/pca_note_amazon_gas_neu_uro.RData")

# examine quality
plot_pc1_pc2(pca_note$x, color = y_true)
plot_pc1_pc2(pca_amazon$x, color = y_true)


# pca using note ===============================================================
# Plot samples in pc1 - pc2 space. Spot 3 clusters and add line boundary to 
# seperate the clusters. The boundaries only good for the saved pca
pca <- pca_note$x
plot_pc1_pc2(pca)
a1 <- 8
a2 <- 0.8
a3 <- -2
curve(a1 * x, from = 0, to = 0.4, add = TRUE, lty = 2)
curve(a2 * x, from = 0, to = -0.4, add = TRUE, lty = 2)
curve(a3 * x, from = 0, to = 0.4, add = TRUE, lty = 2)

pc1 <- pca[, 1]
pc2 <- pca[, 2]
cluster1 <- (pc2 >= a1 * pc1) & (pc2 > a2 * pc1)
cluster2 <- (pc2 <= a2 * pc1) & (pc2 < a3 * pc1)
cluster3 <- (pc2 >= a3 * pc1) & (pc2 < a1 * pc1)

# assigne a label to each cluster
y_clusters <- rep(999, length(y_true))
y_clusters[cluster1] <- 1
y_clusters[cluster2] <- 2
y_clusters[cluster3] <- 3

table(y_true, y_clusters)
# As we know the true labels y_true, we can use them to determine each cluster
# identified by PCA
y_pred_note_pca <- best_match(y_true, y_clusters)
y_cluster_note_pca <- y_clusters


# pca amazon_me ================================================================
# PCA on tfidf generated with Amazon Comprehend Medical named entities. Keep 
# only PC1 and PC2 for clustering visualization and analysis
pca <- pca_amazon$x

# Plot samples in pc1 - pc2 space. Spot 3 clusters and add line boundary to 
# seperate the clusters
plot_pc1_pc2(pca)
a1 <- -0.8
a2 <- 3
a3 <- 0.8
curve(a1 * x, from = 0, to = 0.7, add = TRUE, lty = 2)
curve(a2 * x, from = 0, to = 0.6, add = TRUE, lty = 2)
curve(a3 * x, from = 0, to = -0.4, add = TRUE, lty = 2)

pc1 <- pca[, 1]
pc2 <- pca[, 2]
cluster1 <- (pc2 >= a1 * pc1) & (pc2 < a2 * pc1)
cluster2 <- (pc2 >= a2 * pc1) & (pc2 > a3 * pc1)
cluster3 <- (pc2 <= a3 * pc1) & (pc2 < a1 * pc1)


y_clusters <- rep(999, length(y_true))
y_clusters[cluster1] <- 1
y_clusters[cluster2] <- 2
y_clusters[cluster3] <- 3

# assigne a label to each cluster
y_pred_amazon_pca <- best_match(y_true, y_clusters)
y_cluster_amazon_pca <- y_clusters


# save pca results for shiny ==================================================
save(y_pred_note_pca, y_cluster_note_pca, 
     y_pred_amazon_pca, y_cluster_amazon_pca,
     file = "shiny-apps/RData/pca_results.RData")



