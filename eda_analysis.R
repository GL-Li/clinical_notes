library(data.table)
library(magrittr)
library(stringr)
dt <- fread("~/data/health-care-data/medical-transcriptions-kaggle/mtsamples.csv",
            header = TRUE) %>%
    .[, V1 := NULL]

# categories in transcription
bbb = str_replace_all(dt$transcription, "\\.", " ") %>% 
    str_remove_all(" []+ ") %>%
    str_squish() %>%
    str_extract_all("[A-Z][A-Z ]{2,}[A-Z]")

ccc = unlist(bbb)
