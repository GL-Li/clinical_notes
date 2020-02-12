library(shiny)
library(DT)

server <- function(input, output, session){
    # overview
    source("./controllers/clinical_notes.R", local = TRUE)
    
    # bag of words and text analysis
    source("./controllers/bow_text.R", local = TRUE)
    
    # word cloud
    source("./controllers/word_cloud.R", local = TRUE)
    
    # clustering
    source("./controllers/clustering.R", local = TRUE)
}