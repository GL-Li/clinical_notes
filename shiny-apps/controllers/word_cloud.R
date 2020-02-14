type_1 <- reactive(input$cloud_type_1)
col_1 <- reactive(input$cloud_method_1)

type_2 <- reactive(input$cloud_type_2)
col_2 <- reactive(input$cloud_method_2)

output$wordcloud_1 <- renderCachedPlot(
    {
        # type <- input$cloud_type_1
        # col < input$cloud_method_1
        word_count <- get_word_count(type_1(), col_1())
        #min_freq <- ifelse(col == "Both", 10, 3)
        
        # add a point to make the cloud look better
        if (type_1() == "Neurology" & col_1() == "top_tfidf"){
            tmp <- data.table(word = "", count = 30)
            word_count <- rbindlist(list(tmp, word_count))
        }
        
        set.seed(1234)  # reproducible cloud
        par(mar = rep(0, 4))  # set wordcloud margin to 0
        wordcloud(words = word_count[, word],         
                  freq = word_count[, count],       
                  scale = c(3.5, 0.3),    
                  max.words = 300,
                  min.freq = 2,                  
                  rot.per = 0.2,                
                  random.order=FALSE, 
                  colors=brewer.pal(8, "Dark2"))
        
    },
    bg="transparent",
    cacheKeyExpr = {list(input$cloud_type_1, input$cloud_method_1)}
)

output$wordcloud_2 <- renderCachedPlot(
    {
        # type <- input$cloud_type_2
        # col = input$cloud_method_2
        word_count <- get_word_count(type_2(), col_2())
        #min_freq <- ifelse(col == "Both", 10, 3)
        
        # add a point to make the cloud look better. The maximum count was 9
        # too small compared to 33 in Gastroentoogy 
        if (type_2() == "Neurology" & col_2() == "top_tfidf"){
            tmp <- data.table(word = "", count = 30)
            word_count <- rbindlist(list(tmp, word_count))
        }
        
        set.seed(1234)  # reproducible cloud
        par(mar = rep(0, 4))  # set wordcloud margin to 0, base plot
        wordcloud(words = word_count[, word],         
                  freq = word_count[, count],       
                  scale = c(3.5, 0.3),  
                  max.words = 300,
                  min.freq = 2,                  
                  rot.per = 0.2,                
                  random.order=FALSE, 
                  colors=brewer.pal(8, "Dark2"))
        
    },
    bg="transparent",
    cacheKeyExpr = {list(input$cloud_type_2, input$cloud_method_2)}
)

output$bar_1 <- renderPlot(
    {
        word_count <- get_word_count(type_1(), col_1()) %>%
            .[1:10]
        ggplot(word_count, aes(word, count)) +
            geom_col(fill = "grey80") +
            geom_text(aes(word, count, label = count), hjust = 1, color = "gray20") +
            scale_y_continuous(expand = c(0, 0)) +
            coord_flip() +
            labs(x = NULL,
                 y = NULL,
                 title = "Count of Top 10 Words") +
            theme(panel.background = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size = 12))
    }, 
    bg = "transparent"
)

output$bar_2 <- renderPlot(
    {
        word_count <- get_word_count(type_2(), col_2()) %>%
            .[1:10]
        ggplot(word_count, aes(word, count)) +
            geom_col(fill = "grey80") +
            geom_text(aes(word, count, label = count), hjust = 1, color = "gray20") +
            scale_y_continuous(expand = c(0, 0)) +
            coord_flip() +
            labs(x = NULL,
                 y = NULL,
                 title = "Count of Top 10 Words") +
            theme(panel.background = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size = 12))
    }, 
    bg = "transparent"
)