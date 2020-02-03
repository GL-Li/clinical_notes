# bag of words =================================================================
bow_proxy <- dataTableProxy("bows")

output$bows <- DT::renderDataTable({
    note_bows
},
rownames = FALSE,
#filter = list(position = 'top', clear = FALSE),
options = list(
    columnDefs = list(list(className = 'dt-left', targets = "_all")),
    pageLength = 5,
    processing=FALSE,
    searchHighlight = TRUE
))

observeEvent(input$word, {
    updateSearch(bow_proxy, keywords = list(global = input$word, columns = NULL))
})

# displace stats of a word =====================================================
output$word_stats <- renderText({
    wd <- input$word
    if (nchar(wd) > 0){
        word <- word_stats %>%
            filter(word == wd) 
        
        n_doc <- word %>%
            pull(n_documents)
        n_time <- word %>%
            pull(n_times)
        avg_tf <- word %>%
            pull(avg_tf)
        avg_tfidf <- word %>%
            pull(avg_tfidf)
        if(is_empty(n_doc)){
            n_doc <- 0
            n_time <- 0
            avg_tf <- 0
            avg_tfidf <- 0
        }
    } else {
        n_doc <- 0
        n_time <- 0
        avg_tf <- 0
        avg_tfidf <- 0
    }
    
    #h2(str(count))
    paste0("<p>",
           "Appears in ", 
           "<font size='5'>", n_doc, "</font>", " documents ",
           "<font size='5'>", n_time, "</font>", " times ",
           "and has ",
           "<font size='5'>", avg_tf, "</font>", " average term frequency ",
           "<font size='5'>", avg_tfidf, "</font>", " average TFIDF",
           "</p>")
})