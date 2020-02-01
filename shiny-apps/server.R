library(shiny)

server <- function(input, output){
    # to be filled
    output$search_table <- DT::renderDataTable({
        mt  %>%
            .[, .(sample_type, note = medical_transcription, mt_headers)]
    },
    filter = list(position = 'top', clear = FALSE),
    options = list(
        pageLength = 10,
        processing=FALSE,
        searchHighlight = TRUE,
        search = list(regex = TRUE, caseInsensitive = TRUE)
    ))
    
    output$tfidf <- DT::renderDataTable({
        tfidf
    },
    filter = list(position = 'top', clear = FALSE),
    options = list(
        pageLength = 10,
        processing=FALSE,
        searchHighlight = TRUE,
        search = list(caseInsensitive = FALSE)
    ))
}