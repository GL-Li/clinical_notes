library(tensorflow)
library(keras)
source("utilities.R")
library(data.table)
library(progress)

get_pretrained_matrix <- function(tokens, pretrained_file){
  # Calculate pretrained matrix for the top max_words tokens in a corpus
  #
  # Arguments:
  #   tokens: string vector, tokens tokenized using keras
  #   pretrained_file: str, csv file of saved pretrained matrix, from which to
  #     subset and get the pretrained matrix of tokens
  #   max_words: int, number of top frequent token to keep
  #
  # Return:
  #   a matrix
  
  # load the saved pretrained matrix and keep only those in tokens
  # make sure all tokens are included in pretrained vacob, usually the case
  pretrained_dt <- fread(pretrained_file)
  stopifnot(length(setdiff(tokens, pretrained_dt$V1)) == 0) 
  pretrained_dt <- pretrained_dt[V1 %in% tokens] 
  
  # reorder the pretrained matrix so the row order agrees with words order
  cat("Calculate pretrained matrix:\n")
  order_factor <- factor(tokens, levels = tokens)
  pb <- progress_bar$new(total = length(tokens))
  for (wd in tokens){
    pb$tick()
    pretrained_dt[V1 == wd, levels := which(levels(order_factor) == wd)]
  }
  
  # only return the matrix 
  pretrained_matrix <- pretrained_dt[order(levels)] %>%
    .[, V1 := NULL] %>%
    .[, levels := NULL] %>%
    as.matrix()
}


cnn_metrics <- function(corpus, pretrained_file, seq_length = 500){
  # calculate average accuracy and f1 score out of 100 repeat
  # 
  # Arguments:
  #   corpus: string vector
  #   pretrained_file: str, csv file of saved pretrained matrix, from which to
  #     subset and get the pretrained matrix of tokens
  #
  # Return:
  #   numeric vector 
  #
  
  # initialize tokenizer specifing maximum words
  max_words <- 3000
  tk <- text_tokenizer(num_words = max_words)
  # update tk in place with a vector or list of documents
  fit_text_tokenizer(tk, corpus)
  
  token_index <- tk$word_index
  tokens <- names(token_index)[1:max_words]
  pretrained_matrix <- get_pretrained_matrix(tokens, pretrained_file)
  
  # convert the documents into a list of sequence
  X <- texts_to_sequences(tk, corpus)
  
  # pad the sequence to get a matrix
  seq_length <- seq_length
  X <- pad_sequences(X, seq_length)
  y_class <- dat$y
  n_class <- length(unique(y_class))
  y <- to_categorical(y_class, n_class)
  
  
  n_rep <- 100
  df_acc_f1 <- data.frame(
    acc = numeric(n_rep),
    f1_gas = numeric(n_rep),
    f1_neu = numeric(n_rep),
    f1_uro = numeric(n_rep)
  )
  set.seed(6789)
  in_trains <- caret::createDataPartition(y_class, times = n_rep, p = 0.7)
  
  cat("Train 100 models:\n")
  pb <- progress_bar$new(total = 100)
  pb$tick(0)   # display progress bar right away. 
  for (i in 1:100){
    pb$tick()
    in_train <- in_trains[[i]]
    X_train <- X[in_train,]
    y_train <- y[in_train,] 
    X_test <- X[-in_train,]
    y_test <- y[-in_train,]
    y_test_class <- y_class[-in_train]
    
    dim_emb <- 200
    dropout <- 0.3
    model <- keras_model_sequential() %>% 
      layer_embedding(input_dim = max_words,
                      output_dim = dim_emb,
                      input_length = seq_length) %>%
      layer_dropout(dropout) %>%
      layer_conv_1d(filters = 16,
                    kernel_size = 3,
                    #activation = "relu",
                    padding = "valid",
                    strides = 1) %>%
      layer_dropout(dropout) %>%
      layer_global_average_pooling_1d() %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dropout(dropout) %>%
      # output layer
      layer_dense(n_class, activation = "softmax")
    
    # use pretrained embedding
    get_layer(model, index = 1) %>% 
      set_weights(list(pretrained_matrix))
    
    # compile, fit, and evaluate model in place
    compile(model,
            loss = "categorical_crossentropy",
            optimizer = "adam",
            metrics = "accuracy"
    )
    
    fit(model,
        x = X_train, y = y_train,
        epochs = 50,
        batch_size = 32,
        validation_split = 0.3,
        verbose = 3
    )
    
    
    y_pred <- predict(model, X_test)
    y_pred_class <- predict_classes(model, X_test)
    tb <- table(y_test_class, y_pred_class)
    acc <- sum(diag(tb)) / length(y_test_class)
    
    f1 <- function(tb, k){
      recall <- diag(tb)[k] / sum(y_test_class == k - 1)
      precision <- diag(tb)[k] / sum(y_pred_class == k - 1)
      f1 <- 2 * (recall * precision) / (recall + precision)
    }
    
    f1_gas <- f1(tb, 1)
    f1_neu <- f1(tb, 2)
    f1_uro <- f1(tb, 3)
    
    df_acc_f1[i, ] <- c(acc, f1_gas, f1_neu, f1_uro)
  }
  return(df_acc_f1)
}



# prepare data =================================================================
set.seed(12345)
dat <- read_notes(
  "data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
  duplicate_rm = T,
  cols_keep = "all",
  y_label = TRUE
)


# try out cnn construction using clinical notes ================================
notes <- dat$note
# initialize tokenizer specifing maximum words
max_words <- 3000
tk <- text_tokenizer(num_words = max_words)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)

clinical_word_index <- tk$word_index
clinical_words <- names(clinical_word_index)[1:max_words]

# convert the documents into a list of sequence
X <- texts_to_sequences(tk, notes)

# pad the sequence to get a matrix
seq_length <- 500
X <- pad_sequences(X, seq_length)
y_class <- dat$y
n_class <- length(unique(y_class))
y <- to_categorical(y_class, n_class)

# split X and y into train and test
set.seed(1234)
in_train <- caret::createDataPartition(y_class, p = 0.7, list = FALSE)
X_train <- X[in_train,]
y_train <- y[in_train,] 
X_test <- X[-in_train,]
y_test <- y[-in_train,]
y_test_class <- y_class[-in_train]


# build model =================================================================
dim_emb <- 200  # vector length of pretrained embedding
dropout <- 0.3
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words,
                  output_dim = dim_emb,
                  input_length = seq_length) %>%
  layer_dropout(dropout) %>%
  layer_conv_1d(filters = 16,
                kernel_size = 3,
                #activation = "relu",
                padding = "valid",
                strides = 1) %>%
  layer_dropout(dropout) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(dropout) %>%
  # output layer
  layer_dense(n_class, activation = "softmax")

summary(model)


# set the embedding layer to pretrained matrix
pretrained_file <- "data/gas_neu_uro_token_embeddings_note.csv"
pretrained_matrix <- get_pretrained_matrix(words, pretrained_file)
get_layer(model, index = 1) %>% 
  set_weights(list(pretrained_matrix))

# compile, fit, and evaluate model in place
compile(model,
        loss = "categorical_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
)

fit(model,
    x = X_train, y = y_train,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 3
)


# repeat 100 times to get average metrics =======

# mean and standard deviation
pretrained_file <- "data/gas_neu_uro_token_embeddings_note.csv"
df_acc_f1 <- cnn_metrics(dat$note, pretrained_file)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

pretrained_file <- "data/gas_neu_uro_token_embeddings_amazon.csv"
df_acc_f1 <- cnn_metrics(dat$amazon_me, pretrained_file)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)
