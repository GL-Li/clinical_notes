# take input from text area or file upload, whichever updated =================

input_val <- reactiveValues()
observeEvent(input$text_input, {
    input_val$notes <- input$text_input %>%
        str_split("\n") %>%
        unlist()
})
observeEvent(input$file_upload, {
   dat <- read.csv(
       input$file_upload$datapath, sep = "\n", 
       header = FALSE,
       stringsAsFactors = FALSE
   )
   input_val$notes <- dat$V1
})

# use the reactive value inside reactive environment
new_tfidf <- reactive({
    tfidf <- input_val$notes %>%
        get_dtm(train_vectorizer) %>%
        transform_tfidf(tfidf_model)
})

# new_tfidf <- reactive({
#     tfidf <- notes() %>%
#         get_dtm(train_vectorizer) %>%
#         transform_tfidf(tfidf_model)
# })

# new_tfidf <- reactive({
#     tfidf <- input$text_input %>%
#         str_split("\n") %>%
#         unlist() %>%
#         get_dtm(train_vectorizer) %>%
#         transform_tfidf(tfidf_model)
# })

new_pca <- reactive({
    # use drop = FALSE so one row is still a matrix not a vector
    pca <- predict(pca_model, new_tfidf())[, 1:25, drop = FALSE]
})

prediction <- reactive({
    predict(svm_model_deploy, new_pca())
})




output$print_prediction <- renderPrint({
    input$submit_text_input
    isolate(prediction())
})


# output$prediction <- renderText(
#     {
#         prediction <- predict(svm_model_deploy, new_pca())
#         print(paste("Predicted class is: ", prediction))
#     }
# )


# .. download prediction ====
y_pred <- reactive({
    if (is.factor(prediction())){
        y_pred <- as.numeric(as.character(prediction()))
    } else {
        y_pred <- prediction()
    }
})
output$download <- downloadHandler(
    filename = function() {"prediction.txt"},
    content = function(file) {
        write(y_pred(), file)
    }
)

# file upload ==================================================================
