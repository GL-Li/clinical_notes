# medical specialties in the order of class 0, 1, 2, 3, 4, 5 for convert numbers
# back to strings
lookup_table <- c(
    "Cardiovascular / Pulmonary",
    "Gastroenterology",
    "Obstetrics / Gynecology",
    "Neurology",
    "Orthopedic",
    "Urology"
)

# take input from text area or file upload, whichever updated =================
input_val <- reactiveValues()
observeEvent(input$text_input, {
    input_val$notes <- input$text_input %>%
        str_split("\n") %>%
        unlist()
})
observeEvent(input$file_upload, {
   dat <- read.csv(
       input$file_upload$datapath, 
       sep = "\n", 
       header = FALSE,
       stringsAsFactors = FALSE
   )
   
   input_val$notes <- dat$V1
})

# # use the reactive value inside reactive environment
# new_tfidf <- reactive({
#     texts <- input_val$notes
#     tfidf <- texts %>%
#         get_dtm(train_vectorizer) %>%
#         transform_tfidf(tfidf_model)
#     
#     stopifnot(length(texts) == dim(tfidf)[1])
#     
#     tfidf
# })


new_pca <- reactive({
    texts <- input_val$notes
    dtm <- get_dtm(texts, train_vectorizer)
    tfidf <- transform_tfidf(dtm, tfidf_model)
    # use drop = FALSE so one row is still a matrix not a vector
    pca <- predict(pca_model, tfidf)[, 1:25, drop = FALSE]
})


prediction <- reactive({
    predict(svm_model_deploy, new_pca())
})


output$print_prediction <- renderPrint({
    # + 1 as class start from 0, complicated coversion in case of factor
    lookup_table[as.numeric(as.character(prediction())) + 1]
})


# .. download prediction ====
y_download <- reactive({
    pred <- as.numeric(as.character(prediction())) + 1
    lookup_table[pred]
})
output$download <- downloadHandler(
    filename = function() {"prediction.txt"},
    content = function(file) {
        write(y_download(), file)
    }
)

