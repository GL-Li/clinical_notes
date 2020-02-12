library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(caret)
library(progress)
library(dendextend)

load("RData/medical_entities_bow.RData")
load("RData/clustering.RData")

# get word count 
get_word_count <- function(type, col){
    bow <- note_bows[specialty == type, get(col)]
    if (type == "Both"){
        bow = note_bows[, get(col)]
    }
    count <- bow %>%
        str_split(" ") %>%
        unlist() %>%
        table() %>%
        as.data.table() %>%
        set_colnames(c("word", "count")) %>%
        .[word != ""] %>%    # medaCy generate nothing from some notes
        .[order(-count)] %>%
        .[, word := factor(word, levels = word)]
}
