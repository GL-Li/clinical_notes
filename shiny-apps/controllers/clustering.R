
output$kmeans_img <- renderPlot(
    {
        par(mar = rep(0, 4))
        image(cos_sim * 256, 
              asp = 1,
              bty = "n",
              col = gray(seq(0, 1, length = 256)))
    },
    bg = "transparent",
    height = function() {
        session$clientData$output_kmeans_img_width
    }
)


output$dend <- renderPlot(
    {
        par(mar = c(0, 0, 2, 0))
        plot(dend, main = "Medical Notes Clustering",
             leaflab = "none", yaxt = "none")
        rect.hclust(hc, 2, border = "gray97")
    },
    bg = "transparent"
)

