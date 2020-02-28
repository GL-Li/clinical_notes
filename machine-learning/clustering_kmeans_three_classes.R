source("utilities.R")

# define functions ============================================================
best_kmeans <- function(tfidf, k = 3, pca = TRUE, n_pca = NULL, iter = 100,
                        n_rep = 100){
    # Get best kmeans clustering of a tfidf matrix which has smallest total 
    # withinss
    #
    # Arguments:
    #   tfidf, matrix, normalized tfidf matrix of corpus
    #   k: int, number of clusters
    #   pca: bool, whether or not preprocess tfidf with pca
    #   n_pca: int, first n_pca components used for clustering, default all
    #   iter: int, maximum iteration of kmeans
    #   n_rep: number of repeat time to search for best kmeans model
    # Return:
    #   a kmeans model
    
    if (isTRUE(pca)){
        tfidf <- prcomp(tfidf)$x
    }
    if (!is.null(n_pca)){
        stopifnot(isTRUE(pca))
        stopifnot(is.integer(as.integer(n_pca)))
        tfidf <- tfidf[, 1:n_pca]
    }
    
    set.seed(12345)
    # # km_para to record seed number and withinss of each try
    # km_para <- data.frame(rand_seed = rep(0, n_rep),
    #                       withinss = rep(0, n_rep))
    rand_seeds <- sample(1:10000, n_rep)
    best_withinss <- Inf  # total withinss
    
    pb <- progress_bar$new()
    for (i in 1:n_rep){
        pb$tick()
        rand_seed <- rand_seeds[i]
        set.seed(rand_seed)
        km <- kmeans(tfidf, k, iter.max = iter)
        withinss <- km$tot.withinss
        # km_para[i, ] <- c(rand_seed, withinss)
        
        if (best_withinss > withinss){
            best_withinss <- withinss
            best_rand_seed <- rand_seed
        }
    }
    set.seed(best_rand_seed)
    km <- kmeans(tfidf, k, iter.max = iter)
    return(km)
}

# best km functions with pca ===================================================
best_km <- function(corpus_name, n_pca = NULL){
    # Identify specialties with best kmeans
    # 
    # Arguments:
    #   corpus_name: string, "note" or "amazon", for clinical notes and 
    #     Amazone medical entities
    #   n_pca: int, use the first n_pca components for kmeans
    # Return:
    #   int vector of labels like c(0, 2, 1, 1, 0, ...)
    
    if (is.null(n_pca)){
        if (corpus_name == "note"){
            mtx <- tfidf_note
        } else if (corpus_name == "amazon"){
            mtx <- tfidf_amazon
        }
    } else {
        if (corpus_name == "note"){
            mtx <- pca_note$x[, 1:n_pca]
        } else if (corpus_name == "amazon") {
            mtx <- pca_amazon$x[, 1:n_pca]
        }
    }
    
    km <- best_kmeans(mtx)
    y_clusters <- km$cluster
    
    y_pred <- best_match(y_true, y_clusters)
}

# prepare data ================================================================
# 
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


# # select the best number of clusters ===========================================
# # results: no clear elbow to decide number of clusters. use prior knowledge
# # as we know it is three
# K = 10
# inertia <- c()
# for (k in 1:K){
#     print(k)
#     km <- kmeans(tfidf_amazon, k, iter.max = 100)
#     inertia <- c(inertia, km$tot.withinss)
# }
# plot(1:K, inertia)

# kmeans with clinical notes ==================================================
# no pca, results: accuracy 0.64
y_pred_note_kmeans <- best_km("note")
y_pred_note_kmeans_pca <- best_km("note", n_pca = 25)

par(mfrow = c(2, 2))

plot_pc1_pc2(pca_note$x, 
             pch = y_true, 
             title = "K-means Clusters")
legend("topright", 
       legend = c("cluster 1", "cluster 2", "cluster 3"), 
       pch = c(1, 2, 3),
       cex = 0.8)

plot_pc1_pc2(pca_note$x, 
             color = y_true, 
             title = "True Specialties")
legend("topright", 
       legend = c("Gastroenterology", "Neurology", "Urology"),
       col = c("red", "blue", "cyan"), 
       pch = 1,
       cex = 0.8)

plot_pc1_pc2(pca_note$x, 
             y_pred_note_kmeans, 
             title = "Identified Specialties")
legend("topright", 
       legend = c("Gastroenterology", "Neurology", "Urology"),
       col = c("red", "blue", "cyan"), 
       pch = 1,
       cex = 0.8)

plot_pc1_pc2(prcomp(tfidf_note)$x, 
             color = y_true == y_pred_note_kmeans,
             pch = y_true,
             title = "Compare prediction to true specialties")
legend("topleft", 
       legend = c("Correct", "Wrong"),
       col = c("blue", "red"),
       pch = 16,
       cex = 0.8)
legend("topright", legend = c("Gastroenterology", "Neurology", "Urology"),
       pch = 0:2,
       cex = 0.8)

dev.off()


# kmeans with amazon entities ==================================================
# no pca, results: accuracy 0.64
y_pred_amazon_kmeans <- best_km("amazon")
y_pred_amazon_kmeans_pca <- best_km("amazon", 25)

par(mfrow = c(2, 2))

plot_pc1_pc2(pca_amazon$x, 
             pch = y_clusters, 
             title = "K-means Clusters")
legend("topright", 
       legend = c("cluster 1", "cluster 2", "cluster 3"), 
       pch = c(1, 2, 3),
       cex = 0.8)

plot_pc1_pc2(pca_amazon$x, 
             color = y_true, 
             title = "True Specialties")
legend("topright", 
       legend = c("Gastroenterology", "Neurology", "Urology"),
       col = c("red", "blue", "cyan"), 
       pch = 1,
       cex = 0.8)

plot_pc1_pc2(pca_amazon$x, 
             y_pred_amazon_kmeans_pca, 
             title = "Identified Specialties")
legend("topright", 
       legend = c("Gastroenterology", "Neurology", "Urology"),
       col = c("red", "blue", "cyan"), 
       pch = 1,
       cex = 0.8)

plot_pc1_pc2(pca_amazon$x, 
             color = y_true == y_pred_amazon_kmeans,
             pch = y_true,
             title = "Compare prediction to true specialties")
legend("topleft", 
       legend = c("Correct", "Wrong"),
       col = c("blue", "red"),
       pch = 16,
       cex = 0.8)
legend("topright", legend = c("Gastroenterology", "Neurology", "Urology"),
       pch = 0:2,
       cex = 0.8)

dev.off()





save(y_pred_note_kmeans, y_pred_note_kmeans_pca,
     y_pred_amazon_kmeans, y_pred_amazon_kmeans_pca,
     file = "shiny-apps/RData/kmeans_results.RData")
