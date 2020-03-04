# The raw table of notes =======================================================
output$raw_table <- DT::renderDataTable({
    # only display selected columns
    dat_all <- mtsamples_all[, .(specialty, note, section_headers = sections)]
    
    # only display first sample for each specialty
    dt_first <- dat_all[, .SD[1], by = specialty]
},
rownames = FALSE,
# filter = list(position = 'top', clear = FALSE),
options = list(
    pageLength = 5,
    processing=FALSE
))


# unique specialty count =======================================================
output$specialty_count <- renderPlot({
    # keep only selected medical specialties
    to_keep <- c("Allergy / Immunology", "Autopsy", "Bariatrics",
                 "Cardiovascular / Pulmonary", 
                 "Chiropractic", "Cosmetic / Plastic Surgery", 
                 "Dentistry", "Dermatology", "Diets and Nutritions", 
                 "Endocrinology", "ENT - Otolaryngology", 
                 "Gastroenterology", "Hematology - Oncology", 
                 "Hospice - Palliative Care", 
                 "Nephrology", "Neurology", "Neurosurgery", "Obstetrics / Gynecology", 
                 "Ophthalmology", "Orthopedic", "Pain Management", 
                 "Pediatrics - Neonatal", "Physical Medicine - Rehab", "Podiatry", 
                 "Psychiatry / Psychology", "Rheumatology", "Sleep Medicine", 
                 "Speech - Language", "Urology")
    dat_specialty <- mtsamples_all[specialty %in% to_keep]
    
    # count specialty include duplicates
    count <- sort(table(dat_specialty$specialty), decreasing = TRUE)
    count_top <- count[1:20]

    ggplot() + 
        geom_col(aes(x = factor(names(count_top), levels = names(count_top)),
                     y = as.integer(count_top)),
                 fill = "gray80") +
        geom_text(aes(x = factor(names(count_top), levels = names(count_top)),
                      y = as.integer(count_top),
                      label = as.integer(count_top)),
                  hjust = 1,
                  color = "gray20") +
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = NULL,
             y = NULL,
             title = "Counts of Top 20 Medical Specialties") +
        coord_flip() + 
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank())
})


# Section headers count =========================================================
output$section_count <- renderPlot({
    # remove duplicates but keep the first one
    rows_duplicated <- duplicated(mtsamples_all$note)
    dat_section <- mtsamples_all[!rows_duplicated, .(specialty, note, sections)]
    
    # failed to scrape section from some notes
    sections <- dat_section[sections != "", sections] %>%
        str_split(", ") %>%
        unlist()
    
    # notes have sections
    N <- length(dat_section[sections != "", sections])
    
    # count of each sections
    count <- sort(table(sections), decreasing = TRUE)
    count_top <- count[1:20]
    
    ggplot() + 
        geom_col(aes(x = factor(names(count_top), levels = names(count_top)),
                     y = as.integer(count_top) / N),
                 fill = "gray80") +
        geom_text(aes(x = factor(names(count_top), levels = names(count_top)),
                      y = as.integer(count_top) / N,
                      label = paste0(round(100 * as.integer(count_top) / N, 1), "%")),
                  hjust = 1,
                  color = "gray20") +
        scale_y_continuous(expand = c(0, 0), 
                           labels = scales::percent_format()) +
        labs(x = NULL,
             y = NULL,
             title = "Frequencies of Top 20 Section Headers (As-Is)") +
        coord_flip() + 
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank())
})

