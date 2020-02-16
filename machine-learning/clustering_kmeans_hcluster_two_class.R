library(data.table)
library(magrittr)
library(stringr)
library(tm)
library(caret)
library(progress)
library(dendextend)
source("utilities.R")

dat <- read_notes("data/mtsamples_scraped.csv",
                  specialties = c("Gastroenterology", "Neurology", "Urology"),
                  clean = TRUE,
                  y_label = TRUE)

# tfidf matrix
tfidf <- tfidf_tm(dat$note)
y_true <- dat$y


# preprocess with pca
pca_mtx <- prcomp(tfidf)$x

# how many clusters ============================================================
# tsne analysis https://github.com/jkrijthe/Rtsne 
K = 10
inertia <- c()
for (k in 1:K){
    print(k)
    km <- kmeans(tfidf, k, iter.max = 100)
    inertia <- c(inertia, km$tot.withinss)
}
plot(1:K, inertia)

km <- kmeans(pca_mtx, 3, iter.max = 100)


# how to match cluster to true class
y_clusters <- km$cluster
table(y, y_clusters)

y_pred <- rep(integer(0), length(y))
y_pred[y_clusters == 1] <- 2
y_pred[y_clusters == 2] <- 1
y_pred[y_clusters == 3] <- 0
table(y_true, y_pred)
confusionMatrix(as.factor(y_pred), as.factor(y_true))

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
