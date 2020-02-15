source("./utilities.R")

# get tf and tfidf words
notes <- fread("./data/mtsample_gastroenterology_neurology.csv") %>%
    # missing space after ".", for example "abscess.PROCEDURE"
    .[, note := str_replace_all(note, "\\.", "\\. ")]
tfidf_list <- top_tfidf(notes, "note") 

tfidf <- tfidf_list[["tfidf"]] %>%
    setDT() %>%
    setkey("id")

# save to RData ==========================================
word_stats <- tfidf_list[["word_stats"]]

# get medical entities created by Amazon Comprehend Medical
amazon_me <- fread("./data/amazon_medical_entities.csv") %>%
    setkey("id") %>%
    .[, amazon_me := tolower(amazon_me)]

# get medical entites created by medaCy
medacy_me <- fread("./data/medacy_medical_entities.csv") %>%
    setkey("id") %>%
    .[, medacy_me := tolower(medacy_me)]

# combine all for shiny server to save to RData ==========
note_bows <- tfidf %>%
    medacy_me[.] %>%
    amazon_me[.]


# save to RData ===============================================================
save(note_bows, word_stats, file = "./shiny-apps/RData/medical_entities_bow.RData")
