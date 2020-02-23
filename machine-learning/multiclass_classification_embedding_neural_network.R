library(tensorflow)
library(keras)
source("utilities.R")


# prepare data =================================================================
specialties <- c(
  "Gastroenterology", "Obstetrics / Gynecology",  "Cardiovascular / Pulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
cols <- c("specialty", "note")
dat <- read_notes(
  "data/mtsamples_multi_class.csv", 
  duplicate_rm = TRUE,
  specialties = specialties,
  cols_keep = cols,
  id = TRUE,
  y_label = TRUE
)

# parameter tuning =============================================================
corpus <- dat[, note]
labels <- dat$y
cnn_tune <- function(corpus,
                     labels,
                     max_words = 3000, 
                     seq_length = 500,
                     dim_emb = 64, 
                     dropout = 0.2,
                     n_filters = 16){
  # train and evaluate a cnn model using word embedding
  #
  # Arguments:
  #   corpus: string vector, document to trained
  #   y: integer, labels of documents in corpus
  #   max_word: int, number of words to keep for text_tokenizer()
  #   seq_length: int, length of each document after pad_sequence()
  #   dim_emb: int, length of word embedding
  #   droput: numeric, ratio to drop in dropout layer
  #   n_filters: int, number of filters in layer_conv_1d()
  #
  # Return:
  #   
  tk <- text_tokenizer(num_words = max_words)
  fit_text_tokenizer(tk, notes)
  X <- texts_to_sequences(tk, notes)
  X <- pad_sequences(X, seq_length)
  y_class <- labels
  n_class <- length(unique(y_class))
  y <- to_categorical(y_class, n_class)
  
  # split X and y into train and test
  in_train <- caret::createDataPartition(y_class, p = 0.7, list = FALSE)
  X_train <- X[in_train,]
  y_train <- y[in_train,] 
  X_test <- X[-in_train,]
  y_test <- y[-in_train,]
  y_test_class <- y_class[-in_train]

  model <- keras_model_sequential() %>% 
    layer_embedding(input_dim = max_words,
                    output_dim = dim_emb,
                    input_length = seq_length) %>%
    layer_dropout(dropout) %>%
    layer_conv_1d(filters = n_filters,
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
  
  # summary(model)
  
  # compile, fit, and evaluate model in place
  compile(model,
          loss = "categorical_crossentropy",
          optimizer = "adam",
          metrics = "accuracy"
  )
  
  fit(model,
      x = X_train, 
      y = y_train,
      epochs = 20,
      batch_size = 32,
      validation_split = 0.3,
      verbose = 3
  )
  
  eva <- evaluate(model, X_test, y_test, verbose = 0)
  
  return(eva$accuracy)
}


# tune the parameters
plot_cnn <- function(){
  corpus <- dat[, note]
  labels <- dat$y
  
  n_rep <- 100
  param_df <- data.frame(
    max_words = integer(n_rep),
    seq_length = integer(n_rep),
    dim_emb = integer(n_rep), 
    dropout = numeric(n_rep),
    n_filters = integer(n_rep),
    accuracy = numeric(n_rep)
  )
  
  for (i in 1:n_rep){
    cat(i)
    maxwords <- sample(3000:4000, 1)
    seqlength <- sample(300:400, 1)
    dimemb <- sample(100:200, 1)
    dropout <- sample(1:5/10, 1)
    nfilters <- sample(20:200, 1)
    
    acc <- cnn_tune(
      corpus,
      labels, 
      max_words = maxwords,
      seq_length = seqlength,
      dim_emb = dimemb,
      dropout = dropout,
      n_filters = nfilters
    )
    
    param_df[i, ] <- c(maxwords, seqlength, dimemb, dropout, nfilters, acc)
  }
  return(param_df)
}


# eyeballing the best paramters
# results: 
# first run
#   max_words: 3500   from range 2000:4000
#   seq_length: 350   from range 200:500
#   dim_enb: 120       from range 16:128
#   dropout: 0.4      from range 0.1:0.5
#   n_filters: 32     from range 8:64

# second run
#   max_words: xxxx   from range 3000:4000
#   seq_length: xxx   from range 300:400
#   dim_enb: xx       from range 100:200
#   dropout: xxx      from range 0.1:0.5
#   n_filters: xx     from range 20:100

try_1 <- plot_cnn()
try_2 <- plot_cnn()

plot(param_df$max_words, param_df$accuracy)
plot(param_df$seq_length, param_df$accuracy)
plot(param_df$dim_emb, param_df$accuracy)
plot(param_df$dropout, param_df$accuracy)
plot(param_df$n_filters, param_df$accuracy)


# one model ====================================================================
max_words = 3500
seq_length = 350
dim_emb = 120
dropout = 0.4
n_filters = 32

notes <- dat$note
tk <- text_tokenizer(num_words = max_words)
fit_text_tokenizer(tk, notes)
X <- texts_to_sequences(tk, notes)
X <- pad_sequences(X, seq_length)
y_class <- dat$y
n_class <- length(unique(y_class))
y <- to_categorical(y_class, n_class)

# split X and y into train and test
set.seed(11111)
in_train <- caret::createDataPartition(y_class, p = 0.7, list = FALSE)
X_train <- X[in_train,]
y_train <- y[in_train,] 
X_test <- X[-in_train,]
y_test <- y[-in_train,]
y_test_class <- y_class[-in_train]

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words,
                  output_dim = dim_emb,
                  input_length = seq_length) %>%
  layer_dropout(dropout) %>%
  layer_conv_1d(filters = n_filters,
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

# summary(model)

# compile, fit, and evaluate model in place
compile(model,
        loss = "categorical_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
)

fit(model,
    x = X_train, 
    y = y_train,
    epochs = 20,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 3
)


y_pred <- predict(model, X_test)
y_pred_class <- predict_classes(model, X_test)
table(y_test_class, y_pred_class)

classes_x <- c(
  "Gastroenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
classes_y <- c(
  "Gastro-\nenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
ggplot_multiclass_nn_embedding_recall <- plot_confusion_matrix(y_test_class, y_pred_class, classes_x, classes_y)
ggplot_multiclass_nn_embedding_precision <- plot_confusion_matrix(y_test_class, y_pred_class, classes_x, classes_y, type = "precision")

accuracy_nn_embedding <- accuracy(y_test_class, y_pred_class)


save(ggplot_multiclass_nn_embedding_recall, 
     ggplot_multiclass_nn_embedding_precision,
     accuracy_nn_embedding,
     file = "shiny-apps/RData/ggplot_multiclass_nn_embedding.RData")
