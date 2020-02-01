    library(data.table)
library(tidyverse)
library(tidytext)

mt <- fread("../mtsample_scraped.csv")

get_tfidf <- function(df, col){
    # df: dataframe containing a column of corpus
    # col: string, column name of the corpus selected for tfidf 

    tokens <- as_tibble(df) %>%
        select(!!col) %>%
        mutate(doc_number = 1:nrow(.)) %>% 
        unnest_tokens(word, !!col) %>%
        anti_join(stop_words) %>%
        count(doc_number, word, sort = TRUE)
    total_words <- tokens %>% 
        group_by(doc_number) %>%
        summarise(total = sum(n))
    tfidf <- left_join(tokens, total_words) %>%
        bind_tf_idf(word, doc_number, n)
    sum_tfidf <- tfidf %>%
        group_by(doc_number) %>%
        summarise(sum_tfidf = round(sum(tf_idf), 3)) %>%
        arrange(doc_number) %>%
        cbind(df)
}

tfidf <- get_tfidf(mt[, .(sample_type, medical_transcription)],
                   "medical_transcription")

    