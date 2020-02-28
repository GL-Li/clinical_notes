# render plots and texts for clustering sidebar

# pca ==========================================================================
output$pca_plot <- renderPlot(
    {
        par(mfrow = c(2, 2))
        if (input$pca == "clinical notes"){
            pca <- pca_note$x
            
            plot_pc1_pc2(pca, title = "PCA Clustering")
            a1 <- 8
            a2 <- 0.8
            a3 <- -2
            curve(a1 * x, from = 0, to = 0.4, add = TRUE, lty = 2)
            curve(a2 * x, from = 0, to = -0.4, add = TRUE, lty = 2)
            curve(a3 * x, from = 0, to = 0.4, add = TRUE, lty = 2)

            plot_pc1_pc2(pca, color = y_pred_note_pca, 
                         title = "Identified Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            plot_pc1_pc2(pca, color = y_true, title = "True Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            plot_pc1_pc2(pca, 
                         color = y_true == y_pred_note_pca, 
                         color_map = c("orange", "gray"),
                         pch = y_true,
                         title = "Compare Identified to True Specialties")
            legend("bottomleft", 
                   legend = c("Correct", "Wrong"),
                   col = c("gray", "orange"),
                   pch = 16,
                   cex = 0.8)
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   pch = 0:2,
                   cex = 0.8)
            
        } else if (input$pca == "amazon medical entities"){
            pca <- pca_amazon$x
            plot_pc1_pc2(pca, title = "PCA Clustering")
            a1 <- -0.8
            a2 <- 3
            a3 <- 0.8
            curve(a1 * x, from = 0, to = 0.7, add = TRUE, lty = 2)
            curve(a2 * x, from = 0, to = 0.6, add = TRUE, lty = 2)
            curve(a3 * x, from = 0, to = -0.4, add = TRUE, lty = 2)

            plot_pc1_pc2(pca, color = y_pred_amazon_pca, 
                         title = "Identified Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)

            plot_pc1_pc2(pca, color = y_true, title = "True Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            plot_pc1_pc2(pca, 
                         color = y_true == y_pred_amazon_pca, 
                         color_map = c("orange", "gray"),
                         pch = y_true,
                         title = "Compare Identified Prediction to True Specialties")
            legend("bottomleft", 
                   legend = c("Correct", "Wrong"),
                   col = c("gray", "orange"),
                   pch = 16,
                   cex = 0.8)
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   pch = 0:2,
                   cex = 0.8)
        }
        
    }
)



# hierarchical clustering =====================================================

output$dend_plot <- renderPlot(
    {
        if (input$dend == "clinical notes"){
            plot_dend(hc_note, title = "Dendrogram colored with true specialties")
        } else if (input$dend == "amazon medical entities"){
            plot_dend(hc_amazon, title = "Dendrogram colored with true specialties")
        }
    }
)

output$hcluster_plot <- renderPlot(
    {
        par(mfrow = c(1, 2))
        if (input$dend == "clinical notes"){
            pca <- pca_note$x
            plot_pc1_pc2(pca, color = y_true, title = "True Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            plot_pc1_pc2(pca, 
                         color = y_pred_note_hcluster, 
                         title = "Identified Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            # plot_pc1_pc2(pca, 
            #              color = y_true == y_pred_note_hcluster, 
            #              color_map = c("orange", "gray"),
            #              pch = y_true,
            #              title = "Compare Prediction to True Specialties")
            # legend("bottomleft", 
            #        legend = c("Correct", "Wrong"),
            #        col = c("gray", "orange"),
            #        pch = 16,
            #        cex = 0.8)
            # legend("bottomright", 
            #        legend = c("Gastroenterology", "Neurology", "Urology"),
            #        pch = 0:2,
            #        cex = 0.8)
        } else if (input$dend == "amazon medical entities"){
            pca <- pca_amazon$x
            plot_pc1_pc2(pca, color = y_true, title = "True Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            plot_pc1_pc2(pca, 
                         color = y_pred_amazon_hcluster, 
                         title = "Identified Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
        }
    }
)


# kmeans =======================================================================
output$kmeans_plot <- renderPlot(
    {
        par(mfrow = c(1, 2))
        if (input$kmeans == "clinical notes"){
            pca <- pca_note$x
            plot_pc1_pc2(pca, color = y_true, title = "True Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            if (input$kmeans_pca == "yes"){
                y_pred <- y_pred_note_kmeans_pca
            } else {
                y_pred <- y_pred_note_kmeans
            }
            
            plot_pc1_pc2(pca, 
                         color = y_pred, 
                         title = "Identified Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
        } else {
            pca <- pca_amazon$x
            plot_pc1_pc2(pca, color = y_true, title = "True Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
            
            if (input$kmeans_pca == "yes"){
                y_pred <- y_pred_amazon_kmeans_pca
            } else {
                y_pred <- y_pred_amazon_kmeans
            }
            
            plot_pc1_pc2(pca, 
                         color = y_pred, 
                         title = "Identified Specialties")
            legend("bottomright", 
                   legend = c("Gastroenterology", "Neurology", "Urology"),
                   col = c("red", "blue", "cyan"), 
                   pch = 1,
                   cex = 0.8)
        }
    }
)



