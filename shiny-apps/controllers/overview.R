# The raw table of notes =======================================================
output$raw_table <- renderDataTable({
    note_bows[, .(id, sample_type, medical_note)]
},
rownames = FALSE,
# filter = list(position = 'top', clear = FALSE),
options = list(
    pageLength = 5,
    processing=FALSE
))

# boxplot and scatter plot of sum_tfidf ========================================
output$boxplot_sum_tfidf <- renderPlot(
    ggplot(sum_tfidf, aes(sample_type, sum_tfidf)) + 
        geom_boxplot() + 
        geom_jitter(width = 0.15) +
        xlab("Sample Type") +
        ylab("Sum of TFIDF") +
        ggtitle("Sum of TFIDF of a Document")
)

