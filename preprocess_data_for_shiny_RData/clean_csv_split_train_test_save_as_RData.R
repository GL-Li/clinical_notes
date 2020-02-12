library(data.table)
library(magrittr)

# keep only 3 columns of all scraped data
dat_all <- fread("./data/mtsample_scraped.csv") %>%
    .[, .(specialty, note, sections)]



save(dat_all, file = "./shiny-apps/RData/dat_all.RData")

