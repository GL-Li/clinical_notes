library(data.table)
library(tidyverse)
library(tidytext)

# define function to extract top words using term frequency and tfidf
get_tfidf <- function(df, col){
    # df: dataframe containing a column of corpus
    # col: string, column name of the corpus selected for tfidf 
    
    # count of a word in a document
    tokens <- as_tibble(df) %>%
        select(id, !!col) %>%
        unnest_tokens(word, !!col) %>%
        anti_join(stop_words) %>%
        count(id, word, sort = TRUE) %>%
        # keep words with letters only
        filter(str_detect(word, "^[a-z]+$"))
    
    # times of appearance of a word
    n_times <- tokens %>%
        group_by(word) %>%
        summarise(n_times = sum(n))
    
    # words only shows up one time
    words_1 <- n_times %>%
        filter(n_times == 1) %>%
        select(word) %>%
        pull()
    
    # remove the one-time words, which are note represenative
    tokens <- tokens %>%
        filter(!word %in% words_1)
    
    # total words in a document    
    total_words <- tokens %>% 
        group_by(id) %>%
        summarise(total = sum(n))
    
    df_tfidf <- left_join(tokens, total_words) %>%
        bind_tf_idf(word, id, n)
    
    # stats of a word
    word_stats <- df_tfidf %>%
        group_by(word) %>%
        summarise(n_documents = n(),
                  n_times = sum(n),
                  avg_tf = round(mean(tf),4),
                  avg_tfidf = round(mean(tf_idf), 4))
    
    # top 10 words by term frequency in each document
    top_tf <- df_tfidf %>%
        arrange(desc(tf)) %>%
        group_by(id) %>%
        slice(1:10) %>%
        select(id, word) %>%
        group_by(id) %>%
        summarise(top_tf = paste(word, collapse = " "))
    
    # top 10 words by tfidf
    top_tfidf <- df_tfidf %>%
        arrange(desc(tf_idf)) %>%
        group_by(id) %>%
        slice(1:10) %>%
        select(id, word) %>%
        group_by(id) %>%
        summarise(top_tfidf = paste(word, collapse = " "))
    
    
    sum_tfidf <- df_tfidf %>%
        group_by(id) %>%
        summarise(sum_tfidf = round(sum(tf_idf), 3)) %>%
        left_join(top_tf) %>%
        left_join(top_tfidf) %>%
        right_join(df) %>%
        arrange(id)

    return(list(tfidf = sum_tfidf, word_stats = word_stats))
}

# get tf and tfidf words
notes <- fread("mtsample_gastroenterology_neurology.csv") %>%
    # missing space after ".", for example "abscess.PROCEDURE"
    .[, medical_note := str_replace_all(medical_note, "\\.", "\\. ")]
tfidf_list <- get_tfidf(notes, "medical_note") 

sum_tfidf <- tfidf_list[["tfidf"]] %>%
    setDT() %>%
    setkey("id")

# save to RData ==========================================
word_stats <- tfidf_list[["word_stats"]]

# get medical entities created by Amazon Comprehend Medical
amazon_me <- fread("amazon_medical_entities.csv") %>%
    setkey("id") %>%
    .[, amazon_me := tolower(amazon_me)]

# get medical entites created by medaCy
medacy_me <- fread("medacy_medical_entities.csv") %>%
    setkey("id") %>%
    .[, medacy_me := tolower(medacy_me)]

# combine all for shiny server to save to RData ==========
note_bows <- sum_tfidf %>%
    medacy_me[.] %>%
    amazon_me[.]


# create data for word cloud
# get_word_count <- function(vector_col){
#     count <- vector_col %>%
#         str_split(" ") %>%
#         unlist() %>%
#         table() %>%
#         as.data.table() %>%
#         set_colnames(c("word", "count"))
# }
# amazon_count <- get_word_count(amazon_me[, amazon_me])
# medacy_count <- get_word_count(medacy_me[, medacy_me])
# top_tf_count <- get_word_count(note_bows[, top_tf])
# top_tfidf_count <- get_word_count(note_bows[, top_tfidf])


# save to RData ===============================================================
save(note_bows, file = "../RData/saved.RData")
