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

