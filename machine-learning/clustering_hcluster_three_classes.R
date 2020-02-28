library(dendextend)

source("utilities.R")

# # Prepare data =================================================================
# dat <- read_notes("data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
#                   cols_keep = c("id", "amazon_me", "specialty", "note"),
#                   clean = TRUE,
#                   y_label = TRUE)
# 
# # tfidf matrix
# tfidf_note <- tfidf_tm(dat$note)
# tfidf_amazon <- tfidf_tm(dat$amazon_me)
# y_true <- dat$y

# load the same data for pca, kmeans, and hcluster
load("shiny-apps/RData/pca_note_amazon_gas_neu_uro.RData")



# hierarchical clustering note =================================================
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
# https://uc-r.github.io/hc_clustering 
tfidf = tfidf_note
cos_sim <- tfidf %*% t(tfidf)
dist <- as.dist(1 - cos_sim)
# ward.D and ward.D2 are good for clustering, slight difference
hc_note <- hclust(dist, "ward.D")
#hc <- hclust(dist, "ward.D2")  # one more correct
# all below not good
# hc <- hclust(dist, "single")
# hc <- hclust(dist, "complete")
# hc <- hclust(dist, "average")
# hc <- hclust(dist, "mcquitty")
# hc <- hclust(dist, "median")
# hc <- hclust(dist, "centroid")


# plot dendrogram
# https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html#introduction
dend <- as.dendrogram(hc_note)

# use true y to assign color
sample_colors <- rep(character(0), nrow(tfidf))
sample_colors[y_true == 0] <- "red"
sample_colors[y_true == 1] <- "blue"
sample_colors[y_true == 2] <- "cyan"

dend <- assign_values_to_leaves_edgePar(
    dend=dend, 
    value = sample_colors[order.dendrogram(dend)], 
    edgePar = "col"
)
par(mar = c(0, 0, 2, 0))
plot(dend, main = "Medical Notes Clustering",
     leaflab = "none", yaxt = "none")
legend("topright", 
       legend = c("Gastroenterology", "Neurology", "Urology"),
       cex = 0.8,
       lty = 1,
       col = c("red", "blue", "orange"),
       bty = "n")
rect.hclust(hc_note, 3, border = "lightgreen")
dev.off()

# confusion matrix
y_cluster_note_hcluster <- cutree(hc_note, 3)

y_pred_note_hcluster <- best_match(y_true, y_cluster_note_hcluster)


# hierarchical clustering amazon =================================================
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
# https://uc-r.github.io/hc_clustering 
tfidf = tfidf_amazon
cos_sim <- tfidf %*% t(tfidf)
dist <- as.dist(1 - cos_sim)
# ward.D and ward.D2 are good for clustering, slight difference
hc_amazon <- hclust(dist, "ward.D")
#hc <- hclust(dist, "ward.D2")  # one more correct
# all below not good
# hc <- hclust(dist, "single")
# hc <- hclust(dist, "complete")
# hc <- hclust(dist, "average")
# hc <- hclust(dist, "mcquitty")
# hc <- hclust(dist, "median")
# hc <- hclust(dist, "centroid")


# plot dendrogram
# https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html#introduction
dend <- as.dendrogram(hc_amazon)

# use true y to assign color
sample_colors <- rep(character(0), nrow(tfidf))
sample_colors[y_true == 0] <- "red"
sample_colors[y_true == 1] <- "blue"
sample_colors[y_true == 2] <- "cyan"

dend <- assign_values_to_leaves_edgePar(
    dend=dend, 
    value = sample_colors[order.dendrogram(dend)], 
    edgePar = "col"
)
par(mar = c(0, 0, 2, 0))
plot(dend, main = "Medical Notes Clustering",
     leaflab = "none", yaxt = "none")
legend("topright", 
       legend = c("Gastroenterology", "Neurology", "Urology"),
       cex = 0.8,
       lty = 1,
       col = c("red", "blue", "orange"),
       bty = "n")
rect.hclust(hc_amazon, 3, border = "lightgreen")
dev.off()

# confusion matrix
y_cluster_amazon_hcluster <- cutree(hc_amazon, 3)

y_pred_amazon_hcluster <- best_match(y_true, y_cluster_amazon_hcluster)


save(hc_note, y_cluster_note_hcluster, y_pred_note_hcluster,
     hc_amazon, y_cluster_amazon_hcluster, y_pred_amazon_hcluster,
     file = "shiny-apps/RData/hcluster_results.RData")

# plot_dend ====================================================================
plot_dend <- function(hc){
    # plot dendgram of hierarchical clustering
    # hc: hclust created with function hc <- hclust(dist, "ward.D")
    dend <- as.dendrogram(hc)
    
    # use true y to assign color
    sample_colors <- rep(NA, length(hc$labels))
    sample_colors[y_true == 0] <- "red"
    sample_colors[y_true == 1] <- "blue"
    sample_colors[y_true == 2] <- "orange"
    
    dend <- assign_values_to_leaves_edgePar(
        dend=dend, 
        value = sample_colors[order.dendrogram(dend)], 
        edgePar = "col"
    )
    
    par(mar = c(0, 0, 2, 0))
    plot(dend, main = "Medical Notes Clustering",
         leaflab = "none", yaxt = "none")
    legend("topright", 
           legend = c("Gastroenterology", "Neurology", "Urology"),
           cex = 0.8,
           lty = 1,
           col = c("red", "blue", "orange"),
           bty = "n")
    rect.hclust(hc, 3, border = "lightgreen")
}

