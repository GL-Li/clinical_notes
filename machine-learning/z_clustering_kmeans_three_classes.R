library(data.table)
library(magrittr)
library(stringr)
library(tm)
library(caret)
library(progress)
library(dendextend)
library(plotly)

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


# test how many clusters =======================================================
# tsne analysis https://github.com/jkrijthe/Rtsne 
tfidf = tfidf_amazon
K = 10
inertia <- c()
for (k in 1:K){
    print(k)
    km <- kmeans(tfidf, k, iter.max = 100)
    inertia <- c(inertia, km$tot.withinss)
}
plot(1:K, inertia)


# use amaxon_me, assume we know 3 clusters =====================================
tfidf = tfidf_amazon
set.seed(3721)
km <- kmeans(tfidf, 3, iter.max = 100)

y_clusters = km$cluster
me_1 <- dat$amazon_me[y_clusters == 1]
me_2 <- dat$amazon_me[y_clusters == 2]
me_3 <- dat$amazon_me[y_clusters == 3]

count_1 <- word_count(me_1)[1:100]
count_2 <- word_count(me_2)[1:100]
count_3 <- word_count(me_3)[1:100]

compare_count <- data.frame(me_1 = count_1,
                            me_2 = count_2,
                            me_3 = count_3)

# how to match cluster to true class
y_clusters <- km$cluster
table(y_true, y_clusters)
# y_clusters
# y     1   2   3
#   0  76  87  67
#   1 202   0  20
#   2  51   0 103
# best match would be
# 1 --> 1, 2 --> 0, 3 --> 2
y_clusters[y_clusters == 3] <- 0
y_clusters[y_clusters == 1] <- 1
y_clusters[y_clusters == 2] <- 2

table(y_true, y_clusters)
caret::confusionMatrix(as.factor(y_clusters), as.factor(y_true))

# kmeans clustering ============================================================
# https://uc-r.github.io/kmeans_clustering
kmeans_metrics <- function(tfidf, iter=100, n_rep=1){
    # repeat kmeans to get average metrics based on known y label
    pred <- rep(0, nrow(dt))
    
    pb <- progress_bar$new(total = n_rep)
    for (i in 1:n_rep){
        pb$tick()
        k <- kmeans(dt, 2, iter.max = iter)
        pred_1 <- k$cluster
        pred_2 <- (3 - pred_1) %% 3   # 1 --> 2 and 2 --> 1
        # as kmeans randomly assign 1 and 2 to clusters, we will only take the 
        # one with higher F1 value
        f <- function(pred){
            cm <- confusionMatrix(as.factor(pred), as.factor(target))
            cm$byClass["F1"]
        } 
        f1 <- f(pred_1)
        f2 <- f(pred_2)
        
        if (f1 > f2) {
            pred_0 <- pred_1
        } else {
            pred_0 <- pred_2
        }
        
        pred <- pred + pred_0 / n_rep
    }
    pred <- round(pred)
    confusionMatrix(as.factor(pred), as.factor(target))
}



# tsne ========================================================================
set.seed(123)
tsne <- Rtsne(tfidf, theta = 0.5)
plot(tsne$Y, col = as.factor(y), asp = 1)


# pca ==========================================================================
set.seed(123)
pca <- prcomp(tfidf)
plot(pca)
plot(pca$x[, 1], pca$x[, 2], col = y + 1)

# 3D plot
p <- plot_ly(as.data.frame(pca$x), x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(y), colors = c('red', 'green', "black")) %>%
    add_markers(size = 1) %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
                        yaxis = list(title = 'PC2'),
                        zaxis = list(title = 'PC3')))

p




# tfidf -- must normalize each sample to vector length 1
tfidf_mtx <- df_tm[["tfidf_matrix"]]
tfidf_norm <- tfidf_mtx / sqrt(rowSums(tfidf_mtx * tfidf_mtx))
get_kmeans(tfidf_norm)

# tf
tf_mtx <- df_tm[["tf_matrix"]]
tf_norm <- tf_mtx / sqrt(rowSums(tf_mtx * tf_mtx))
get_kmeans(tf_norm)

# hierarchical clustering ======================================================
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
# https://uc-r.github.io/hc_clustering 
set.seed(1234)
tfidf = tfidf_note
cos_sim <- tfidf %*% t(tfidf)
par(mar = rep(0, 4))
image(cos_sim * 256, col = gray(seq(0, 1, length = 256)))
image(cos_sim * 256, col = rgb(seq(0, 1, length = 256), 0, 0))

dist <- as.dist(1 - cos_sim)
# ward.D and ward.D2 are good for clustering, slight difference
hc <- hclust(dist, "ward.D")
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
dend <- as.dendrogram(hc)

# use true y to assign color
sample_colors <- rep(character(0), nrow(tfidf))
sample_colors[y == 0] <- "red"
sample_colors[y == 1] <- "blue"
sample_colors[y == 2] <- "orange"

dend <- assign_values_to_leaves_edgePar(
    dend=dend, 
    value = sample_colors[order.dendrogram(dend)], 
    edgePar = "col"
)
par(mar = c(0, 0, 2, 0))
plot(dend, main = "Medical Notes Clustering",
     leaflab = "none", yaxt = "none")
rect.hclust(hc, 3, border = "green")



# confusion matrix
clustering <- cutree(hc, 2)
confusionMatrix(as.factor(clustering), as.factor(target))

# save RData ==================================================================
save(tfidf_norm, cos_sim, dend, hc, clustering, target,
     file = "./shiny-apps//RData/clustering.RData")

