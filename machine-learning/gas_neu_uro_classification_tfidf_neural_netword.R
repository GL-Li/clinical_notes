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

# try out different nn construction ============================================
# result:
# simple two dense layer with drop out regularization works just fine

notes <- dat$note
# initialize tokenizer specifing maximum words
tk <- text_tokenizer(num_words = 3000)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
# convert the documents into a matrix of tfidf
X <- texts_to_matrix(tk, notes, mode = "tfidf")
# normalize the matrix so that length of each row vector is 1
X <- X / sqrt(rowSums(X * X))

# for multiclass, y should be converted to a matrix 
y_class <- dat$y
n_class <- length(unique(y_class))
y <- to_categorical(y_class, n_class)

# split X and y into train and test
set.seed(1234)
in_train <- caret::createDataPartition(y_class, p = 0.7, list = FALSE)
X_train <- X[in_train,]
y_train <- y[in_train,] 
X_test <- X[-in_train,]
y_test <- y[-in_train]
y_test_class <- y_class[-in_train]


model <- keras_model_sequential() %>% 
  # input layer
  layer_dense(32, input_shape = dim(X_train)[2], activation = "relu") %>%
  layer_dropout(0.2) %>% 
  # second layer
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  # output layer
  layer_dense(n_class, activation = "softmax")

#summary(model)

# compile, fit, and evaluate model in place
compile(model,
        loss = "categorical_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
)

fit(model,
    x = X_train, y = y_train,
    epochs = 20,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 3
)

y_pred <- predict(model, X_test)
y_pred_class <- predict_classes(model, X_test)
table(y_test_class, y_pred_class)


# define function to get average metrics =====================================
nn_metrics <- function(corpus, n_rep = 100, pca = FALSE, n_pca = 25,  epoch = 20){
  # calculate average accuracy and f1 score out of 100 repeat
  # 
  # Arguments:
  #   corpus: string, "note" or "amazon_me"
  #   pca: bool, if TRUE, process with pca
  #   n_pca: int, number of pc to keep
  #   epoch: int, epoch for model training, manual set after a few plot
  #
  # Return:
  #   numeric vector 
  #
  
  notes <- dat[, get(corpus)]
  # initialize tokenizer specifing maximum words
  tk <- text_tokenizer(num_words = 3000)
  # update tk in place with a vector or list of documents
  fit_text_tokenizer(tk, notes)
  # convert the documents into a matrix of tfidf
  X <- texts_to_matrix(tk, notes, mode = "tfidf")
  # normalize the matrix so that length of each row vector is 1
  X <- X / sqrt(rowSums(X * X))
  
  y_class <- dat$y
  n_class <- length(unique(y_class))
  y <- to_categorical(y_class, n_class)
  
  if (pca){
    X <- prcomp(X)$x[, 1:n_pca]
  }
  
  df_acc_f1 <- data.frame(
    acc = numeric(n_rep),
    f1_gas = numeric(n_rep),
    f1_neu = numeric(n_rep),
    f1_uro = numeric(n_rep)
  )
  set.seed(6789)
  in_trains <- caret::createDataPartition(y_class, times = n_rep, p = 0.7)
  for (i in 1:n_rep){
    cat(i)
    in_train <- in_trains[[i]]
    X_train <- X[in_train,]
    y_train <- y[in_train,] 
    X_test <- X[-in_train,]
    y_test <- y[-in_train,]
    y_test_class <- y_class[-in_train]
    
    model <- keras_model_sequential() %>% 
      # input layer
      layer_dense(32, input_shape = dim(X_train)[2], activation = "relu") %>%
      layer_dropout(0.2) %>% 
      # second layer
      layer_dense(units = 16, activation = "relu") %>% 
      layer_dropout(0.2) %>% 
      # output layer
      layer_dense(n_class, activation = "softmax")
    
    #summary(model)
    
    # compile, fit, and evaluate model in place
    compile(model,
            loss = "categorical_crossentropy",
            optimizer = "adam",
            metrics = "accuracy"
    )
    
    fit(model,
        x = X_train, y = y_train,
        epochs = epoch,
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
    
    # assert correct calculation
    stopifnot(acc <= 1 & f1_gas <= 1 & f1_neu <= 1 & f1_uro <= 1)
    stopifnot(acc >= 0 & f1_gas >= 0 & f1_neu >= 0 & f1_uro >= 0)
    
    df_acc_f1[i, ] <- c(acc, f1_gas, f1_neu, f1_uro)
  }
  return(df_acc_f1)
}

# find the best n_pca ==========================================================
# result:
# For note, n_pca = 40 - 60 is best. choose 50
# for amazon_me, 20- 40, choose 30
n_pcas <- c(2:50, 2 * (26:50), 5 * (21:45))
n_rep <- length(n_pcas)
acc <- numeric(n_rep)

for (i in 1:n_rep){
  npca <- n_pcas[i]
  df_acc_f1 <- nn_metrics("amazon_me", n_rep = 1, pca = TRUE, n_pca = npca, epoch = 40)
  acc[i] <- sapply(df_acc_f1, mean)[1]
  print(paste(npca, acc[i]))
}

plot(n_pcas[1:70], acc[1:70])



# repeat 100 times to get average metrics ======================================

# mean and standard deviation
df_acc_f1 <- nn_metrics("note")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- nn_metrics("amazon_me")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- nn_metrics("note", pca = TRUE, n_pca = 50, epoch = 40)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- nn_metrics("amazon_me", pca = TRUE, n_pca = 30, epoch = 40)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)



