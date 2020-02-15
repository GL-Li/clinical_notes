source("utilities.R")

# all mt samples as scraped ====================================================
mtsamples_all <- fread("./data/mtsamples_scraped.csv")

save(mtsamples_all, file = "./shiny-apps/RData/mtsamples_all.RData")


# word statisctics including n_documents, n_time, avg_tf, avg_tfidf ============
# only for three selected specialties 
notes <- fread("data/mtsamples_gastroenterology_neurology_urology.csv")
tfidf_list <- top_tfidf(notes, "note") 
word_stats <- tfidf_list[["word_stats"]]

save(word_stats, file = "shiny-apps/RData/word_stats.RData")


# add amazon_me, medacy_me, top_tf and top_tfidf ===============================
# to the three selected specialties
tfidf <- tfidf_list[["tfidf"]] %>%
    setDT()

# get medical entities created by Amazon Comprehend Medical and medaCy
amazon_medacy <- fread("data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv") %>%
    .[, .(id, amazon_me, medacy_me)]

# combine all for shiny server to save to RData
note_bows <- amazon_medacy[tfidf, on = "id"]

save(note_bows, file = "./shiny-apps/RData/note_bows.RData")
