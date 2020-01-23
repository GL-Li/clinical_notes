library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)

mt <- fread("mtsample_scraped.csv", header = TRUE)

# headers used in transcription ===============================================
# all headers
headers <- mt$mt_headers %>%
    str_split(", ") %>%
    unlist() %>%
    .[!. == ""]

# count of each headers
header_count <- sort(table(headers), decreasing = TRUE)

header_top <- header_count[1:100]

ggplot() + 
    geom_col(aes(x = factor(names(header_top), levels = names(header_top)),
                 y = as.integer(header_top))) +
    coord_flip()

# Word cloud
# plot