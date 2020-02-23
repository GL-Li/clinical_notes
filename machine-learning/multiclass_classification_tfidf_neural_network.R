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
notes <- dat$note


# try out different nn construction ============================================
# result:
# simple two dense layer with drop out regularization works just fine

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

# one model tfidf ==============================================================
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
set.seed(11111)
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

classes_x <- c(
  "Gastroenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
classes_y <- c(
  "Gastro-\nenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
ggplot_multiclass_nn_tfidf_recall <- plot_confusion_matrix(y_test_class, y_pred_class, classes_x, classes_y)
ggplot_multiclass_nn_tfidf_precision <- plot_confusion_matrix(y_test_class, y_pred_class, classes_x, classes_y, type = "precision")

accuracy_nn_tfidf <- accuracy(y_test_class, y_pred_class)




# one model pca ==============================================================

# initialize tokenizer specifing maximum words
tk <- text_tokenizer(num_words = 3000)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
# convert the documents into a matrix of tfidf
X <- texts_to_matrix(tk, notes, mode = "tfidf")
# normalize the matrix so that length of each row vector is 1
X <- X / sqrt(rowSums(X * X))

X <- prcomp(X)$x[, 1:25]

# for multiclass, y should be converted to a matrix 
y_class <- dat$y
n_class <- length(unique(y_class))
y <- to_categorical(y_class, n_class)

# split X and y into train and test
set.seed(11111)
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
    epochs = 30,
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
ggplot_multiclass_nn_pca_recall <- plot_confusion_matrix(y_test_class, y_pred_class, classes_x, classes_y)
ggplot_multiclass_nn_pca_precision <- plot_confusion_matrix(y_test_class, y_pred_class, classes_x, classes_y, type = "precision")

accuracy_nn_pca <- accuracy(y_test_class, y_pred_class)




# save for shiny
save(ggplot_multiclass_nn_pca_recall, ggplot_multiclass_nn_pca_precision,
     ggplot_multiclass_nn_tfidf_recall, ggplot_multiclass_nn_tfidf_precision,
     accuracy_nn_tfidf, accuracy_nn_pca,
     file = "shiny-apps/RData/ggplot_multiclass_nn.RData")


