output$multiclass_1 <- renderPlot(
    {
        if (input$cm_method_1 == "svm + tfidf"){
            g <- get(paste0(
                "ggplot_multiclass_svm_tfidf_", input$cm_type_1
            ))
        } else if (input$cm_method_1 == "svm + tfidf + pca"){
            g <- get(paste0(
                "ggplot_multiclass_svm_pca_", input$cm_type_1
            ))
        } else if (input$cm_method_1 == "xgboost + tfidf"){
            g <- get(paste0(
                "ggplot_multiclass_xgb_tfidf_", input$cm_type_1
            ))
        } else if (input$cm_method_1 == "xgboost + tfidf + pca"){
            g <- get(paste0(
                "ggplot_multiclass_xgb_pca_", input$cm_type_1
            ))
        } else if (input$cm_method_1 == "neural network + tfidf"){
            g <- get(paste0(
                "ggplot_multiclass_nn_tfidf_", input$cm_type_1
            ))
        } else if (input$cm_method_1 == "neural network + tfidf + pca"){
            g <- get(paste0(
                "ggplot_multiclass_nn_pca_", input$cm_type_1
            ))
        } else if (input$cm_method_1 == "neural network + embedding"){
            g <- get(paste0(
                "ggplot_multiclass_nn_embedding_", input$cm_type_1
            ))
        }
        g
    }
)

output$multiclass_2 <- renderPlot(
    {
        if (input$cm_method_2 == "svm + tfidf"){
            g <- get(paste0(
                "ggplot_multiclass_svm_tfidf_", input$cm_type_2
            ))
        } else if (input$cm_method_2 == "svm + tfidf + pca"){
            g <- get(paste0(
                "ggplot_multiclass_svm_pca_", input$cm_type_2
            ))
        } else if (input$cm_method_2 == "xgboost + tfidf"){
            g <- get(paste0(
                "ggplot_multiclass_xgb_tfidf_", input$cm_type_2
            ))
        } else if (input$cm_method_2 == "xgboost + tfidf + pca"){
            g <- get(paste0(
                "ggplot_multiclass_xgb_pca_", input$cm_type_2
            ))
        } else if (input$cm_method_2 == "neural network + tfidf"){
            g <- get(paste0(
                "ggplot_multiclass_nn_tfidf_", input$cm_type_2
            ))
        } else if (input$cm_method_2 == "neural network + tfidf + pca"){
            g <- get(paste0(
                "ggplot_multiclass_nn_pca_", input$cm_type_2
            ))
        } else if (input$cm_method_2 == "neural network + embedding"){
            g <- get(paste0(
                "ggplot_multiclass_nn_embedding_", input$cm_type_2
            ))
        }
        g
    }
)


output$acc_1 <- renderText(
    {
        if (input$cm_method_1 == "svm + tfidf"){
            acc <- accuracy_svm_tfidf
        } else if (input$cm_method_1 == "svm + tfidf + pca"){
            acc <- accuracy_svm_pca
        } else if (input$cm_method_1 == "xgboost + tfidf"){
            acc <- accuracy_xgb_tfidf
        } else if (input$cm_method_1 == "xgboost + tfidf + pca"){
            acc <- accuracy_xgb_pca
        } else if (input$cm_method_1 == "neural network + tfidf"){
            acc <- accuracy_nn_tfidf
        } else if (input$cm_method_1 == "neural network + tfidf + pca"){
            acc <- accuracy_nn_pca
        } else if (input$cm_method_1 == "neural network + embedding"){
            acc <- accuracy_nn_embedding
        }
        
        acc <- round(acc * 100, 1)
        paste0("<p>",
               "Overall accuracy: ", 
               "<font size='5'>", acc, "%", "</font>",
               "</p>")
    }
)


output$acc_2 <- renderText(
    {
        if (input$cm_method_2 == "svm + tfidf"){
            acc <- accuracy_svm_tfidf
        } else if (input$cm_method_2 == "svm + tfidf + pca"){
            acc <- accuracy_svm_pca
        } else if (input$cm_method_2 == "xgboost + tfidf"){
            acc <- accuracy_xgb_tfidf
        } else if (input$cm_method_2 == "xgboost + tfidf + pca"){
            acc <- accuracy_xgb_pca
        } else if (input$cm_method_2 == "neural network + tfidf"){
            acc <- accuracy_nn_tfidf
        } else if (input$cm_method_2 == "neural network + tfidf + pca"){
            acc <- accuracy_nn_pca
        } else if (input$cm_method_2 == "neural network + embedding"){
            acc <- accuracy_nn_embedding
        }
        
        acc <- round(acc * 100, 1)
        paste0("<p>",
               "Overall accuracy: ", 
               "<font size='5'>", acc, "%", "</font>",
               "</p>")
    }
)