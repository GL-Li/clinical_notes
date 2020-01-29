library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

mt <- fread("mtsample_scraped.csv", header = TRUE)

# headers used in transcription ===============================================
# all headers in individual word
headers <- mt$mt_headers %>%
    str_split(", ") %>%
    unlist() %>%
    .[!. == ""]

# count of each headers
header_count <- sort(table(headers), decreasing = TRUE)

# header count distribution
ggplot() + 
    geom_bar(aes(header_count)) +
    xlim(0, 100)

header_top <- header_count[1:50]

ggplot() + 
    geom_col(aes(x = factor(names(header_top), levels = names(header_top)),
                 y = as.integer(header_top))) +
    coord_flip()


wordcloud(names(header_count), header_count)
set.seed(1234)
wordcloud(words = names(header_count), 
          freq = header_count, 
          scale = c(1.5, 0.2),
          min.freq = 5,
          rot.per = 0.2,
          random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))

# group the same headers but in different names ===============================
names(header_count)
# ANESTHESIA
# PROCEDURE
# PREOPERATIVE DIAGNOSIS
# POSTOPERATIVE DIAGNOSIS
# PHYSICAL EXAMINATION
# HISTORY OF PRESENT ILLNESS
# IMPRESSION
# ALLERGIES
# PAST MEDICAL HISTORY
# REVIEW OF SYSTEMS
# SOCIAL HISTORY
# PLAN
# COMPLICATIONS
# MEDICATIONS
# FINDINGS
# FAMILY HISTORY => social history?
# ESTIMATED BLOOD LOSS
# PROCEDURE PERFORMED
# POSTOPERATIVE DIAGNOSES => POSTOPERATIVE DIAGNOSIS (SES -> SIS)
# PREOPERATIVE DIAGNOSES => PREOPERATIVE DIAGNOSIS
# CHIEF COMPLAINT
# ASSESSMENT
# HISTORY      ?? many other histories above
# DESCRIPTION OF PROCEDURE
# 


# medical notes ===============================================================
# select 100 from each of Gastroenterology and Neurology for machine learning
notes = mt[sample_type %in% c("Gastroenterology", "Neurology"), 
          .(sample_type, medical_transcription)] %>%
    .[, .SD[1:100], by = sample_type]

fwrite(notes, file="selected_notes.csv", row.names = FALSE)
