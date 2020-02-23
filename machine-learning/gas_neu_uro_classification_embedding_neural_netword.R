library(tensorflow)
library(keras)
source("utilities.R")

# prepare data =================================================================
set.seed(12345)
dat <- read_notes(
  "data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
  duplicate_rm = T,
  cols_keep = "all",
  y_label = TRUE
)


# try out cnn construction =====================================================
notes <- dat$note
# initialize tokenizer specifing maximum words
max_words <- 3000
tk <- text_tokenizer(num_words = max_words)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
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

dim_emb <- 64
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

# compile, fit, and evaluate model in place
compile(model,
        loss = "categorical_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
)

fit(model,
    x = X_train, y = y_train,
    epochs = 30,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 3
)


# repeat 100 times to get average metrics =======
cnn_metrics <- function(corpus, seq_length = 500){
  # calculate average accuracy and f1 score out of 100 repeat
  # 
  # Arguments:
  #   corpus: string, "note" or "amazon_me"
  #
  # Return:
  #   numeric vector 
  #
  
  notes <- dat[, get(corpus)]
  # initialize tokenizer specifing maximum words
  max_words <- 3000
  tk <- text_tokenizer(num_words = max_words)
  # update tk in place with a vector or list of documents
  fit_text_tokenizer(tk, notes)
  # convert the documents into a list of sequence
  X <- texts_to_sequences(tk, notes)
  
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
  for (i in 1:100){
    cat(i)
    in_train <- in_trains[[i]]
    X_train <- X[in_train,]
    y_train <- y[in_train,] 
    X_test <- X[-in_train,]
    y_test <- y[-in_train,]
    y_test_class <- y_class[-in_train]
    
    dim_emb <- 64
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
    
    # compile, fit, and evaluate model in place
    compile(model,
            loss = "categorical_crossentropy",
            optimizer = "adam",
            metrics = "accuracy"
    )
    
    fit(model,
        x = X_train, y = y_train,
        epochs = 30,
        batch_size = 32,
        validation_split = 0.3,
        verbose = 2
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

# mean and standard deviation
df_acc_f1 <- cnn_metrics("note")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- cnn_metrics("amazon_me", seq_length = 500)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)
