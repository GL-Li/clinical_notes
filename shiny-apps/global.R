library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(caret)
library(progress)
library(dendextend)

# mtsamples_all ===============================================================
# load all mtsamples as scraped for all specialties and then keep three columns 
# for shiny
load("RData/mtsamples_all.RData")
dat_all <- mtsamples_all[, .(specialty, note, sections)]


# word_stats ==================================================================
# load word statistics including n_documents, n_time, avg_tf, avg_tfidf for all
# words in three selected specialites
load("RData/word_stats.RData")

# note_bows ===================================================================
# amazon_me, medacy_me, top_tf, top_tfidf for three selected specialties
load("RData/note_bows.RData")

# get word count for wordcloud plot
get_word_count <- function(type, col){
    bow <- note_bows[specialty == type, get(col)]
    if (type == "All"){
        bow = note_bows[, get(col)]
    }
    count <- tolower(bow) %>%
        str_split(", | ") %>%
        unlist() %>%
        table() %>%
        as.data.table() %>%
        set_colnames(c("word", "count")) %>%
        .[!word %in% tm::stopwords()] %>% # remove stopwords
        .[word != ""] %>%    # medaCy generate nothing from some notes
        .[order(-count)] %>%
        .[count > 1] %>%  # delete useless info to save plotting time
        .[, word := factor(word, levels = word)]
}

# clustering ==============================================================
# load dat, y_true, tfidf, pca for a particular read_notes randomnization to 
# keep data consistencey across pca, hclutering, and kmeans
load("RData/pca_note_amazon_gas_neu_uro.RData")

# load pca results
load("RData/pca_results.RData")

plot_pc1_pc2 <- function(pca, 
                         color = NULL, 
                         color_map = c("red", "blue", "cyan"),
                         pch = NULL,
                         title = NULL){
    # Plot samples in PC1-PC2 space
    #
    # Arguments:
    #   pca: matrix, pca of tfidf
    #   color: int vector to mark the color of each sample. can be y_true, 
    #      y_clusters, y_pred, or other vector of the same length as pca
    #   color_map: string, color to map color
    #   pch: int vector, shape of data point
    #   title: string, plot title
    
    PC1 <- pca[, 1]
    PC2 <- pca[, 2]
    
    sample_colors <- rep("black", nrow(pca))
    if (!is.null(color)){
        sample_colors[color == 0] <- color_map[1]
        sample_colors[color == 1] <- color_map[2]
        sample_colors[color == 2] <- color_map[3]
    }
    
    if (!is.null(color) & !is.null(pch)){
        plot(PC1, PC2, col = sample_colors, pch = pch, main = title)
    } else if (!is.null(color)){
        plot(PC1, PC2, col = sample_colors, main = title)
    } else if (!is.null(pch)){
        plot(PC1, PC2, pch = pch, main = title)
    } else {
        plot(PC1, PC2, main = title)
    }
}

# h_cluster ====================================================================
load("RData/hcluster_results.RData")

plot_dend <- function(hc, title = NULL){
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
    plot(dend, main = title,
         leaflab = "none", yaxt = "none")
    legend("topleft", 
           legend = c("Gastroenterology", "Neurology", "Urology"),
           lty = 1,
           cex = 0.8,
           col = c("red", "blue", "orange"),
           bty = "n")
    rect.hclust(hc, 3, border = "lightgreen")
}


# kmeans =======================================================================
# load kmeans results. pca model using first two PCs
#y_pred_note_kmeans, y_pred_note_kmeans_pca,
# y_pred_amazon_kmeans, y_pred_amazon_kmeans_pca
load("RData/kmeans_results.RData")


# classification ===============================================================
# .. multiclass ====
load("RData/ggplot_multiclass_svm.RData")
load("RData/ggplot_multiclass_xgb.RData")
load("RData/ggplot_multiclass_nn.RData")
load("RData/ggplot_multiclass_nn_embedding.RData")

# .. load trained models ====
model_svm <- readRDS("trained_models/model_svm_pca.rda")
