library(wordcloud2)
library(RColorBrewer)

output$wordcloud_1 <- renderCachedPlot({
    type <- input$cloud_type_1
    col = input$cloud_method_1
    word_count <- get_word_count(type, col)
    #min_freq <- ifelse(col == "Both", 10, 3)
    
    set.seed(1234)  # reproducible cloud
    wordcloud(words = word_count[, word],         
              freq = word_count[, count],       
              scale = c(2, 0.2),    
              max.words = 200,
              #min.freq = 1,                  
              rot.per = 0.2,                
              random.order=FALSE, 
              colors=brewer.pal(8, "Dark2"))

},
cacheKeyExpr = {list(input$cloud_type_1, input$cloud_method_1)})

output$wordcloud_2 <- renderCachedPlot({
    type <- input$cloud_type_2
    col = input$cloud_method_2
    word_count <- get_word_count(type, col)
    #min_freq <- ifelse(col == "Both", 10, 3)
    
    # add a point to make the cloud 
    
    set.seed(1234)  # reproducible cloud
    wordcloud(words = word_count[, word],         
              freq = word_count[, count],       
              scale = c(2, 0.2),  
              max.words = 200,
              #min.freq = 1,                  
              rot.per = 0.2,                
              random.order=FALSE, 
              colors=brewer.pal(8, "Dark2"))
    
},
cacheKeyExpr = {list(input$cloud_type_2, input$cloud_method_2)})