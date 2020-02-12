library(tensorflow)
library(keras)
library(data.table)
library(magrittr)
library(stringr)
source("utilities.R")

# prepare data starting from medical note text
dat <- fread("data/mtsample_gastroenterology_neurology.csv") %>%
  .[, note := str_replace_all(note, "\\.", "\\. ")] %>%
  .[, y := as.integer(factor(specialty)) - 1]

notes <- dat[, note]
# initialize tokenizer specifing maximum words
tk <- text_tokenizer(num_words = 3000)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
# convert the documents into a matrix of tfidf
X <- texts_to_matrix(tk, notes, mode = "tfidf")
# normalize the matrix so that length of each row vector is 1
X <- X / sqrt(rowSums(X * X))
y <- dat[, y]

# split X and y into train and test
set.seed(1234)
in_train <- sample(1:nrow(X), round(0.7 * nrow(X)))
in_test <- setdiff(1:nrow(X), in_train) %>%
  sample()  # to shuffle the row numbers
X_train <- X[in_train,]
y_train <- y[in_train] 
X_test <- X[in_test,]
y_test <- y[in_test]


model <- keras_model_sequential() %>% 
  # input layer
  layer_dense(256, input_shape = dim(X_train)[2], activation = "relu") %>%
  layer_dropout(0.2) %>% 
  # second layer
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  # output layer
  layer_dense(1, activation = "sigmoid")

summary(model)

# compile, fit, and evaluate model in place
compile(model,
        loss = "binary_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
)

fit(model,
    x = X_train, y = y_train,
    epochs = 10,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 2
)

evaluate(model, X_test, y_test, verbose = 0)


# get model metrics use custum defined function
pred <- predict(model, X_test)
metrics_binary(y_test, pred)



save_model_tf(object = model, filepath = "trained_models/binary_tfidf_neural_network")

reloaded_model <- load_model_tf("trained_models/binary_tfidf_neural_network")
all.equal(predict(model,X), predict(reloaded_model, X))
