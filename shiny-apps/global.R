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

# kmeans_cluster ==============================================================


# h_cluster ====================================================================