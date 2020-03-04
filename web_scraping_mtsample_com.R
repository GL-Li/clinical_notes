# last reviewed: 20200211

# To scrape all sample medical transcriptions at mtsamples.com.
# Often scraping with the XPath and css selector returns nothing so we choose 
# to scrape all text and then use regular expression to extract the needed
# data from the text.

library(rvest)
library(stringr)
#library(progress)


# scrape data of one sample
scrape_one_sample <- function(sample_url){
    sample_page <- read_html(sample_url)
    
    # get string of section titles seperated by ",". css selector works here
    sections <- sample_page %>%  
        html_nodes(css = "b") %>%
        html_text() %>%
        str_extract("[A-Z][A-Z /]+[A-Z]") %>%
        #str_remove(":") %>%
        .[!is.na(.)] %>%
        setdiff("NOTE") %>%
        paste(collapse = ", ")
    
    # get all text of the sample. Other contents are extracted from sample_text
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
    description <- str_extract(sample_text_1, "(?<=Description:).+(?=\r\n)") %>%
        str_trim() %>% str_squish()
    
    sample_text_2 <- str_remove(sample_text_1, "^.*\r\n")
    transcription <- str_remove(sample_text_2, "^.+Report\\)") %>%
        str_remove("^[\r\n ]*") %>%
        str_extract("^.*(?=\r\n)") %>%
        str_remove_all("\t") %>%
        str_trim() %>% str_squish()
    
    keywords <- str_extract(sample_text, "(?<=Keywords: \r\n).*(?=\r\n)") %>%
        str_trim() %>% str_squish()
    
    # rename for easy use later
    df <- data.frame(
        specialty = sample_type,  # sample type / medical specialty
        name = sample_name,
        description = description,
        note = transcription,  # clinical note
        sections = sections,
        keywords = keywords,
        stringsAsFactors = FALSE
    )
}
# sample_url <- "https://www.mtsamples.com/site/pages/sample.asp?Type=85-Surgery&Sample=1233-Adenocarcinoma%20&%20Mesothelioma"
# aaa <- scrape_one_sample(sample_url)


# get url to each sample in one page
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
#page_url <- "https://www.mtsamples.com/site/pages/browse.asp?type=21%2DEndocrinology&page=2"
# bbb <- get_sample_urls(page_url)


# scrape all samples in one page
scrape_one_page <- function(page_url){
    df_page <- data.frame(
        specialty = character(0),
        name = character(0),
        description = character(0),
        note = character(0),
        sections = character(0),
        keywords = character(0),
        stringsAsFactors = FALSE
    )
    for(sample_url in get_sample_urls(page_url)){
        df <- scrape_one_sample(sample_url)
        df_page <- rbind(df_page, df)
    }
    return(df_page)
}
# page_url <- "https://www.mtsamples.com/site/pages/browse.asp?type=21%2DEndocrinology&page=2"
# ccc <- scrape_one_page(page_url)


# get the number of pages of a sample type / medical specialty
get_number_pages <- function(type_url){
    text <- read_html(type_url) %>%
        html_node(xpath = '//*[@id="wrapper"]') %>%
        html_text() %>%
        str_remove_all("[\r|\n\t]")
    
    if(str_detect(text, ">\\s+>>")){
        num <- str_extract(text, "[0-9]+(?=\\s+>\\s+>>)") %>%
            as.integer()
        return(num)
    } else {
        return(1)
    }
}
# type_url <- "https://www.mtsamples.com/site/pages/browse.asp?type=85-Surgery"
# ddd <- get_number_pages(type_url)


# get the url of each page of a Sample Type / Medical Specialty using the first 
# page url of a Sample Type Medical specialty
get_page_urls <- function(type_url){
    number_pages <- get_number_pages(type_url)
    if (number_pages == 1){
        page_urls <- type_url
    } else {
        page_urls <- type_url
        for(i in 2:number_pages){
            url_i <- paste0(type_url, "&page=", i)
            page_urls <- c(page_urls, url_i)
        }
    }
    
    return(page_urls)
}
# type_url <- "https://www.mtsamples.com/site/pages/browse.asp?type=85-Surgery"
# eee <- get_page_urls(type_url)


# get the url for the first page of each Sample Type / Medical Specialty from
# https://www.mtsamples.com/
get_type_urls <- function(home_url){
    home_text <- read_html(home_url) %>%
        html_node(xpath = '//*[@id="MenuTypeLeft"]') %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        paste0("https://www.mtsamples.com", .) %>%
        # replace " " with "%20" for legal url
        str_replace_all(" ", "%20")
    return(home_text)
}
# home_url <- "https://www.mtsamples.com/"
# fff <- get_type_urls(home_url)


# scrape all samples from home site
scrape_all_samples <- function(home_url){
    if (!dir.exists("data")){
        cat("create a new directory data/ under current directory to save scraping.")
        dir.create("data")
    }
    
    page_count <- 0
    mt <-  data.frame(
        specialty = character(0),
        name = character(0),
        description = character(0),
        note = character(0),
        sections = character(0),
        keywords = character(0),
        stringsAsFactors = FALSE
    )
    type_urls <- get_type_urls(home_url)
    
    # give an estimated number of total pages to scrape, used to track the 
    # scraping progress. It should be larger than the real number.
    total_pages <- 510
    #pb <- progress_bar$new(total = total_pages)
    for (type_url in type_urls){
        page_urls <- get_page_urls(type_url)
        for (page_url in page_urls){
            #pb$tick()
            page_count <- page_count + 1
            cat(paste0("Scraping page ", page_count, " of ~", total_pages,
                       " --- ", page_url, "\n"))
            df <- scrape_one_page(page_url)
            mt <- rbind(mt, df)
        }
    }
    
    # save the scraped data
    csv_file <- paste0("./data/mtsamples_", str_remove_all(Sys.Date(), "-"), ".csv")
    write.csv(mt, file=csv_file, row.names = FALSE)
    cat(paste0("Scraped data saved successfully to ", csv_file))
}

home_url <- "https://www.mtsamples.com/"
scrape_all_samples(home_url)
