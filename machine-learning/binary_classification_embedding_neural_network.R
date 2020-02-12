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
max_words <- 3000
tk <- text_tokenizer(num_words = max_words)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
# convert the documents into a list of sequence
X <- texts_to_sequences(tk, notes)
# # examine sequence length, the longest is 2471, mean 430
# len <- sapply(X, function(x) length(x))
# summary(len)
# pad the sequence to get a matrix
seq_length <- 500
X <- pad_sequences(X, seq_length)
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

dim_emb <- 32

model <- keras_model_sequential() %>% 
  # input layer
  layer_embedding(input_dim = max_words, 
                  output_dim = dim_emb, 
                  input_length = seq_length) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(filters = 256, 
                kernel_size = 3,
                activation = "relu",
                padding = "valid",
                strides = 1) %>%
  layer_dropout(0.2) %>%
  layer_global_average_pooling_1d() %>%
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
    epochs = 20,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 2
)

evaluate(model, X_test, y_test, verbose = 0)


# get model metrics use custum defined function
pred <- predict(model, X_test)
metrics_binary(y_test, pred)
plot(y_test, pred)


# save tensorflow model, different from traditional ML mode ====================
save_model_tf(object = model, filepath = "trained_models/binary_embedding_neural_network")
reloaded_model <- load_model_tf("trained_models/binary_embedding_neural_network")
all.equal(predict(model,X), predict(reloaded_model, X))
