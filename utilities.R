###
### collection of functions used by multiple files in this project
###

library(tm)
library(magrittr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(tidytext)


read_notes <- function(csv_file, 
                       specialties = NULL,
                       y_label = NULL, 
                       cols_keep = c("specialty", "note"),
                       randomize = TRUE,
                       duplicate_rm = TRUE, 
                       clean = TRUE,
                       id = TRUE){
    # read clinical notes and add label y to the original data
    #
    # Arguments:
    #   csv_file: string, path to the the data file
    #   specialties: string vector, selected specialties such as
    #     c("Gastroenterology", "Neurology")
    #   cols_keep: string, columns in the orignial data to keep, "all" to keep
    #     all columns.
    #   randomize: boolean, randomize row (sample) orders to break grouping
    #   y_label: boolean, if TRUE, add a class label 0, 1, 2, ... to each note
    #   duplicate_rm: boolean, remove rows if duplicated in column note
    #   clean: boolean, if TRUE add missing space after ".", for example,
    #     "abscess.PROCEDURE".
    #   id: boolean, add id to each sample in the original data. Randomize and 
    #     remove duplicates does not change note id.
    #
    # Return:
    #   a data.table
    
    dat <- fread(csv_file)
    if (!is.null(specialties)){
        dat <- dat[specialty %in% specialties]
    }
    if (!identical(cols_keep, "all")){
        dat <- dat[, ..cols_keep]   # ..var select columns by variable
    }
    if (isTRUE(clean)){
        # missing space after ".", for example "abscess.PROCEDURE"
        dat[, note := str_replace_all(note, "\\.", "\\. ")]
    }
    if (isTRUE(y_label)){
        dat[, y := as.integer(factor(specialty)) - 1]
    }
    if (id){
        dat[, id := 1:nrow(dat)]
        setcolorder(dat, c("id", setdiff(names(dat), "id")))
    }
    if (duplicate_rm){
        rows_duplicated <- duplicated(dat$note)
        dat <- dat[!rows_duplicated]
        message(paste("Deleted", sum(rows_duplicated), 
                  "rows with duplicated notes.",
                  "Set duplicate_rm = FALSE if you want to keep duplicates."))
    }
    if (randomize){
        dat <- dat[sample(nrow(dat))]
    }
}



word_count <- function(doc_vector){
    # count word in corpus
    #
    # Arguments:
    #   doc_vector: a vector of documents
    #
    # Return:
    #   data.table
    
    count <- tolower(doc_vector) %>%
        str_split(", | ") %>%
        unlist() %>%
        table() %>%
        as.data.table() %>%
        set_colnames(c("word", "count")) %>%
        .[!word %in% tm::stopwords()] %>% # remove stopwords
        .[word != ""] %>%    # medaCy generate nothing from some notes
        .[order(-count)] %>%
        .[count > 1] %>%  # delete useless info to save plotting time
        .[, word := factor(word, levels = word)]
}



top_tfidf <- function(df, col){
    # Add tfidf columns to a dataframe containing a column of documents and 
    # count the words in documents using package tidytext
    #
    # Arguments:
    #   df: dataframe containing a column of corpus
    #   col: string, column name of the corpus selected for tfidf 
    # return:
    #   list of two data frames:
    #     tfidf: include tf, tfidf, top_tf, top_tfidf
    
    # count of each word in each document
    tokens <- as_tibble(df) %>%
        select(id, !!col) %>%
        # list all tokens in each document
        unnest_tokens(word, !!col) %>%
        # remove stop words. do NOT use tidytext's stop_words, too broad
        filter(!word %in% tm::stopwords()) %>% 
        # keep words with letters and "'" only, then remove 's
        filter(str_detect(word, "^[a-z']+$")) %>%
        mutate(word = str_remove_all(word, "'s")) %>%
        # count grouped by id and word
        count(id, word, sort = TRUE)
        
    
    # times of appearance of each word in all document
    n_times <- tokens %>%
        group_by(word) %>%
        summarise(n_times = sum(n))
    
    # words only shows up one time in all document
    words_1 <- n_times %>%
        filter(n_times == 1) %>%
        select(word) %>%
        pull()
    
    # remove the one-time words, which are not represenative
    tokens <- tokens %>%
        filter(!word %in% words_1)
    
    # nubmer of words in each document    
    total_words <- tokens %>% 
        group_by(id) %>%
        summarise(total = sum(n))
    
    # calculate tfidf and combined with total number of words in each documents
    df_tfidf <- bind_tf_idf(tokens, word, id, n) %>%
        left_join(total_words)

    # for each word: number of documents, total count, average tf and tfidf
    word_stats <- df_tfidf %>%
        group_by(word) %>%
        summarise(n_documents = n(),
                  n_times = sum(n),
                  avg_tf = round(mean(tf),4),
                  avg_tfidf = round(mean(tf_idf), 4))
    
    # top 10 words by term frequency in each document
    top_tf <- df_tfidf %>%
        arrange(desc(tf)) %>%
        group_by(id) %>%
        slice(1:10) %>%
        select(id, word) %>%
        group_by(id) %>%
        summarise(top_tf = paste(word, collapse = " "))
    
    # top 10 words by tfidf in each document
    top_tfidf <- df_tfidf %>%
        arrange(desc(tf_idf)) %>%
        group_by(id) %>%
        slice(1:10) %>%
        select(id, word) %>%
        group_by(id) %>%
        summarise(top_tfidf = paste(word, collapse = " "))
    
    
    tfidf <- top_tf %>%
        left_join(top_tfidf) %>%
        right_join(df) %>%
        arrange(id)
    
    return(list(tfidf = tfidf, word_stats = word_stats))
}

tfidf_tm <- function(corpus, sparsity = 0.992){
    # Calculate normalized tfidf matrix of a coupus using tm package
    #
    # Arguments:
    #   corpus: a vector of text documents
    #   sparsity: fraction of top words to keep
    #
    # Return:
    #   matrix, each row is the mormalized tfidf vector of a document
    
    corpus <- Corpus(VectorSource(corpus)) %>%
        tm_map(tolower) %>%
        tm_map(stripWhitespace) %>%
        # remove stopwords before removing punctuationo so that stopwords like 
        # it's and i'll can be removed
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(removePunctuation) %>%
        tm_map(stemDocument)
    
    tfidf <- DocumentTermMatrix(
        corpus,
        control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
    ) %>%
        removeSparseTerms(sparsity) %>%
        as.matrix() 
    
    # normalize so that each row vector has length of 1
    tfidf <- tfidf / sqrt(rowSums(tfidf * tfidf))
    
    return(tfidf)
}


plot_cv <- function(rep_id = NULL, dat = metrics_cv){
    # Plot the train and validation metrics over iterations of 
    # cross validation that used to find best parameters
    #
    # Arguments:
    #  rep_id: one of the many repeats. If NULL, plot each repeat
    #  dat: list of data frames generated by the cross validation run
    
    if (is.null(rep_id)){
        for (i in 1:length(dat)){
            p <- ggplot(dat[[i]]) +
                geom_line(aes(iter, train_mean), color = "blue") +
                geom_line(aes(iter, test_mean), color = "red") +
                labs(title = paste("repeat", i),
                     x = "Iteration / Epoch",
                     y = "Average Metrics")
            print(p)
            
            key <- readline(
                prompt = paste0("Press [Enter] to view next repeat", 
                                " [Esc] to exit: ")
            )
        }
    } else {
        ggplot(dat[[rep_id]]) +
            geom_line(aes(iter, train_mean), color = "blue") +
            geom_line(aes(iter, test_mean), color = "red") +
            labs(title = paste(rep_id, "repeat"),
                 x = "Iteration / Epoch",
                 y = "Average Metrics")
    }
}


metrics_binary <- function(y_true, y_pred, cutoff = 0.5){
    # Get key metrics of binary classification
    #
    # Arguments:
    #   y_true: integer, true class
    #   y_pred: numeric, predicted probability
    #   cutoff: numeric in 0 - 1, cutoff probability
    # Return:
    #  numeric vector, model metrics auc, f1, sensitity, and specificity
    
    y_pred_class <- round(y_pred)
    auc <- ModelMetrics::auc(y_true, y_pred)
    f1 <- ModelMetrics::f1Score(y_true, y_pred, cutoff)
    sensitivity <- ModelMetrics::sensitivity(y_true, y_pred, cutoff)
    specificity <- ModelMetrics::specificity(y_true, y_pred, cutoff)
    
    cat("confusion matrix:\n       y_true\n")
    print(ModelMetrics::confusionMatrix(y_true, y_pred))
    cat("   \n")
    
    return(c(auc = auc, 
             f1 = f1, 
             sensitivity = sensitivity, 
             specificity = specificity))
}
