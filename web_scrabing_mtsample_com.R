library(rvest)
library(stringr)

# sample_url <- "https://www.mtsamples.com/site/pages/sample.asp?Type=3-Allergy%20/%20Immunology&Sample=386-Allergic%20Rhinitis"
# sample_url <- "https://www.mtsamples.com/site/pages/sample.asp?Type=85-Surgery&Sample=1233-Adenocarcinoma%20&%20Mesothelioma"
# scrabe_one_sample(sample_url) -> aaa

# scrape data of one sample
scrape_one_sample <- function(sample_url){
    sample_page <- read_html(sample_url)
    
    # category in transcribes
    mt_header <- sample_page %>%
        html_nodes(css = "b") %>%
        html_text() %>%
        str_extract("[A-Z][A-Z /]+[A-Z]:") %>%
        str_remove(":") %>%
        .[!is.na(.)] %>%
        setdiff("NOTE") %>%
        paste(collapse = ", ")
    
    sample_text <- sample_page %>%
        html_node(xpath = '//*[@id="sampletext"]') %>%
        html_text() %>%
        # dotall = TRUE to match . to \r and \n
        str_remove(regex("^.+(?=Sample Type)", dotall=TRUE))
    
    # extract everything between "Medical Specialty: " and "Sample Name: "
    sample_type <- str_extract(sample_text, "(?<=Medical Specialty:).+(?=Sample Name:)") %>%
        str_trim() %>% str_squish()
    sample_name <- str_extract(sample_text, "(?<=Sample Name:).+(?=\r\n)") %>%
        str_trim() %>% str_squish()
    
    sample_text_1 <- str_remove(sample_text, "^.*\r\n")
    description <- str_extract(sample_text_1, "(?<=Description).+(?=\r\n)")
    
    sample_text_2 <- str_remove(sample_text_1, "^.*\r\n")
    transcription <- str_remove(sample_text_2, "^.+Report\\)") %>%
        str_remove("^[\r\n ]*") %>%
        str_extract("^.*(?=\r\n)")
    
    keywords <- str_extract(sample_text, "(?<=Keywords: \r\n).*(?=\r\n)")

    
    df <- data.frame(
        medical_specialty = sample_type,
        sample_name = sample_name,
        description = description,
        medical_transcription = transcription,  # mt for medical transcription
        mt_header = mt_header,
        keywords = keywords,
        stringsAsFactors = FALSE
    )
}

# get url to each sample in one page
page_url <- "https://www.mtsamples.com/site/pages/browse.asp?type=21%2DEndocrinology&page=2"
get_sample_urls <- function(page_url){
    sample_urls <- read_html(page_url) %>%
        html_nodes(xpath = '//*[@id="Browse"]') %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        paste0("https://www.mtsamples.com", .) %>%
        # replace " " with "%20" for legal url
        str_replace_all(" ", "%20")
    return(sample_urls)
}
bbb <- get_sample_urls(page_url)

# scrape one page for example this is one page:
# https://www.mtsamples.com/site/pages/browse.asp?type=21%2DEndocrinology&page=2
scrape_one_page <- function(page_url){
    df_page <- data.frame(
        medical_specialty = character(0),
        sample_name = character(0),
        description = character(0),
        medical_transcription = character(0),
        mt_header = character(0),
        keywords = character(0),
        stringsAsFactors = FALSE
    )
    for(sample_url in get_sample_urls(page_url)){
        df <- scrape_one_sample(sample_url)
        df_page <- rbind(df_page, df)
    }
    return(df_page)
}

ccc <- scrape_one_page(page_url)

# get the number of pages of a medical specialty, the first page of a specialty is
# https://www.mtsamples.com/site/pages/browse.asp?type=96-Hematology%20-%20Oncology

specialty_url <- "https://www.mtsamples.com/site/pages/browse.asp?type=96-Hematology%20-%20Oncology"
get_number_pages <- function(specialty_url){
    text <- read_html(specialty_url) %>%
        html_node(xpath = '//*[@id="wrapper"]') %>%
        html_text() %>%
        str_remove_all("[\r|\n\t]")
    
    if(str_detect(text, ">\\s+>>")){
        num <- str_extract(text, "[0-9]+(?=\\s+>\\s+>>)") %>%
            as.integer()
    } else {
        return(1)
    }
}
