library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

load("RData/saved.RData")

# get word count 
get_word_count <- function(type, col){
    bow <- note_bows[sample_type == type, get(col)]
    if (type == "Both"){
        bow = note_bows[, get(col)]
    }
    count <- bow %>%
        str_split(" ") %>%
        unlist() %>%
        table() %>%
        as.data.table() %>%
        set_colnames(c("word", "count")) %>%
        .[order(-count)] %>%
        .[, word := factor(word, levels = word)]
}
