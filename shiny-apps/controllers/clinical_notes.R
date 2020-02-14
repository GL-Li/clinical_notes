# The raw table of notes =======================================================
output$raw_table <- renderDataTable({
    # only display first sample for each specialty
    dat_all[, .SD[1], by = specialty] 
},
rownames = FALSE,
# filter = list(position = 'top', clear = FALSE),
options = list(
    pageLength = 10,
    processing=FALSE
))


# Section headers count =========================================================
output$section_count <- renderPlot({
    # failed to scrape section from some notes
    sections <- dat_all[sections != "", sections] %>%
        str_split(", ") %>%
        unlist()
    
    # notes have sections
    N <- length(dat_all[sections != "", sections])
    
    # count of each sections
    count <- sort(table(sections), decreasing = TRUE)
    count_top <- count[1:20]
    
    ggplot() + 
        geom_col(aes(x = factor(names(count_top), levels = names(count_top)),
                     y = as.integer(count_top) / N),
                 fill = "gray80") +
        geom_text(aes(x = factor(names(count_top), levels = names(count_top)),
                      y = as.integer(count_top) / N,
                      label = paste0(round(100 * as.integer(count_top) / N, 1), "%")),
                  hjust = 1,
                  color = "gray20") +
        scale_y_continuous(expand = c(0, 0), 
                           labels = scales::percent_format()) +
        labs(x = NULL,
             y = NULL,
             title = "Frequencies of Top 20 Section Headers (As-Is)") +
        coord_flip() + 
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank())
})

