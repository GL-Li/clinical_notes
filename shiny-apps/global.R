library(data.table)
library(magrittr)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(dendextend)
library(text2vec)
library(e1071)

# clinical note ===============================================================
# load all mtsamples as scraped for all specialties and then keep three columns 
# for shiny
load("RData/mtsamples_all.RData")

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
        .[!word %in% tm::stopwords()] %>%  # remove stopwords
        .[word != "mg"] %>%  # unit of medication, too common for medacy 
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
    sample_colors[y_true == 2] <- "cyan"
    
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
           col = c("red", "blue", "cyan"),
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
load("RData/multiclass_classification_tfidf_pca_models.RData")
svm_model_deploy <- readRDS("trained_models/svm_model_deploy.rds")


# .. text2vec functions ====
get_iter <- function(corpus, ids = NULL, stem = TRUE){
    # create iterator for text2vec
    #
    # Arguments:
    #   corpus: string vector
    #   ids: id of corpus
    #   stem: bool, use stem tokenizer if TRUE, word tokenizer if not
    #
    # Return:
    #   a text2vec iterator
    #
    
    if (stem){
        tokenizer <- function(x) {
            word_tokenizer(x) %>% 
                lapply( function(x) SnowballC::wordStem(x, language="en"))
        }
    } else {
        tokenizer <- word_tokenizer
    }
    it <- itoken(corpus, tolower, tokenizer, ids = ids)
}


get_vocab <- function(corpus){
    # Crate text2vec vocabularoy of a corpus
    it <- get_iter(corpus)
    vocab <- create_vocabulary(it, stopwords = tm::stopwords())
}


get_vectorizer <- function(corpus){
    # Create text2vec vectorizer from corpus for use in create_dtm
    vocab <- get_vocab(corpus)
    vocab_vectorizer(vocab)
}


get_dtm <- function(corpus, vectorizer){
    # Get dtm of a corpus using existing vectorizer
    it <- get_iter(corpus)
    dtm <- create_dtm(it, vectorizer)
}


fit_tfidf <- function(dtm){
    # create a tfidf model using dtm
    mdl <- TfIdf$new()
    fit_transform(dtm, mdl)  # fit does not work
    return(mdl)
}


transform_tfidf <- function(dtm, tfidf_model){
    # Get normalized tfidf matrix of dtm using tfidf_model
    tfidf <- transform(dtm, tfidf_model)
    tfidf <- as.matrix(tfidf)
    tfidf <- tfidf / sqrt(rowSums(tfidf * tfidf))
}

