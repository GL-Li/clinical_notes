library(data.table)
library(magrittr)
library(stringr)
library(tm)
library(caret)
library(progress)
library(dendextend)

load("./shiny-apps/RData/saved.RData")
note_bows[, target := ifelse(sample_type == "Gastroenterology", 1, 2)]
target <- note_bows$target


# Using the full text tf and tfidf =============================================
# clean up the document
dt <- note_bows[, medical_note := str_replace_all(medical_note, "\\.", " ")]

get_tfidf <- function(dt, col, sparsity = 0.995){
    # make sure dt$col are clean and readable data
    corpus = Corpus(VectorSource(dt[, get(col)])) %>%
        tm_map(tolower) %>%
        tm_map(stripWhitespace) %>%
        # remove stopwords before removing punctuationo so that stopwords like 
        # it's and i'll can be removed
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(removePunctuation) %>%
        tm_map(stemDocument)
    tf_mtx <- DocumentTermMatrix(corpus) %>%
        removeSparseTerms(sparsity) %>%
        as.matrix()
    tf_dt <- tf_mtx %>%
        as.data.table()
    
    tfidf_mtx <- DocumentTermMatrix(
        corpus,
        control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
    ) %>%
        removeSparseTerms(sparsity) %>%
        as.matrix() 
    tfidf_dt <- tfidf_mtx %>%
        as.data.table()
    
    return(list(tf_matrix = tf_mtx,
                tf_datatable = tf_dt,
                tfidf_matrix = tfidf_mtx,
                tfidf_datatable = tfidf_dt))
}

df_tm <- get_tfidf(note_bows, "medical_note")

# kmeans clustering ============================================================
# https://uc-r.github.io/kmeans_clustering
get_kmeans <- function(dt, iter=100, n_rep=100){
    pred <- rep(0, nrow(dt))
    
    pb <- progress_bar$new(total = n_rep)
    for (i in 1:n_rep){
        pb$tick()
        k <- kmeans(dt, 2, iter.max = iter)
        pred <- pred + k$cluster / n_rep
    }
    pred <- round(pred)
    confusionMatrix(as.factor(pred), as.factor(target))
}

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
tfidf_mtx <- df_tm[["tfidf_matrix"]]
tfidf_norm <- tfidf_mtx / sqrt(rowSums(tfidf_mtx * tfidf_mtx))
cos_sim <- tfidf_norm %*% t(tfidf_norm)
par(mar = rep(0, 4))
image(cos_sim * 256, col = gray(seq(0, 1, length = 256)))
image(cos_sim * 256, col = rgb(seq(0, 1, length = 256), 0, 0))

dist <- as.dist(1 - cos_sim)
# ward.D and ward.D2 are good for clustering, slight difference
hc <- hclust(dist, "ward.D")
hc <- hclust(dist, "ward.D2")  # one more correct
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
# labels(dend) <- rep("", 451)
sample_colors <- c(rep("red", 230), rep("blue", 221))
#labels_colors(dend) <- sample_colors[order.dendrogram(dend)]


# plot(dend, main = "xxx", 
#      ylab = "", xlab = "", yaxt = "none")
# rect.hclust(hc, 2, border = "red")


dend2 <- assign_values_to_leaves_edgePar(
    dend=dend, 
    value = sample_colors[order.dendrogram(dend)], 
    edgePar = "col"
)
par(mar = c(0, 0, 2, 0))
plot(dend2, main = "Medical Notes Clustering",
     leaflab = "none", yaxt = "none")
rect.hclust(hc, 2, border = "gray97")



# confusion matrix
clustering <- cutree(hc, 2)
confusionMatrix(as.factor(clustering), as.factor(target))
