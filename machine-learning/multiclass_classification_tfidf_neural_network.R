library(tensorflow)
library(keras)
library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(tidyr)

get_metrics <- function(y_true, y_pred, cutoff = 0.5){
  auc <- ModelMetrics::auc(y_true, y_pred)
  f1 <- ModelMetrics::f1Score(y_true, y_pred, cutoff)
  sensitivity <- ModelMetrics::sensitivity(y_true, y_pred, cutoff)
  specificity <- ModelMetrics::specificity(y_true, y_pred, cutoff)
  
  cat("confusion matrix:\n       y_true\n")
  y_pred <- round(y_pred)
  print(table(y_pred_class, y_true))
  cat("   \n")
  
  return(c(auc = auc, f1 = f1, sensitivity = sensitivity, specificity = specificity))
}

# prepare data starting from medical note text
#file_url <- "https://gl-shared-data.s3.amazonaws.com/mtsamples_multi_class.csv"
dat <- fread("data/mtsamples_multi_class.csv") %>%
  .[, medical_note := str_replace_all(medical_note, "\\.", "\\. ")] %>%
  .[, y := as.integer(factor(sample_type)) - 1]

notes <- dat[, medical_note]
# initialize tokenizer specifing maximum words
tk <- text_tokenizer(num_words = 3000)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
# convert the documents into a matrix of tfidf
X <- texts_to_matrix(tk, notes, mode = "tfidf")
# normalize the matrix so that length of each row vector is 1
X <- X / sqrt(rowSums(X * X))

# for multiclass, y should be converted to a matrix 
y_class <- dat[, y]
n_class <- length(unique(y_class))
y <- to_categorical(y_class, n_class)

# split X and y into train and test
set.seed(1234)
in_train <- sample(1:nrow(X), round(0.7 * nrow(X)))
in_test <- setdiff(1:nrow(X), in_train) %>%
  sample()  # to shuffle the row numbers
X_train <- X[in_train,]
y_train <- y[in_train,] 
X_test <- X[in_test,]
y_test <- y[in_test,]
y_test_class <- y_class[in_test]


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
    epochs = 10,
    batch_size = 32,
    validation_split = 0.3,
    verbose = 2
)

# retrieve validation metrics
val_history <- model$history$history$val_accuracy %>% 
  unlist()
acc <- max(val_history)
epoch <- which.min(val_history)


# evaluate model with test data ================================================
# get model metrics use custum defined function
evaluate(model, X_test, y_test, verbose = 0)

pred <- predict(model, X_test)
pred_class <- predict_classes(model, X_test)


cm <- table(y_test_class, pred_class)  # confusion matrix
n <- nrow(cm)
cm_dt <- as.data.table(matrix(unlist(cm), ncol = 1)) %>%
  .[, x := rep(0:(n-1), n)] %>%
  .[, y := rep(0:(n-1), each = n)]
classes <- unique(dat$sample_type) %>%
  str_replace(" / ", "\n")

ggplot() + 
  geom_jitter(aes(y_test_class, pred_class), color = "blue", size = 1,
              width = 0.15, height = 0.15, alpha = 0.3) +
  geom_text(data = cm_dt, aes(x, y + 0.3, label = V1), color = "red") +
  scale_x_continuous(breaks = 0:6, labels = classes) +
  scale_y_continuous(breaks = 0:6, labels = classes) +
  labs(x = "True Sample Type",
       y = "Predicted Sample Type")



caret::confusionMatrix(as.factor(pred_class), as.factor(y_test_class))
ModelMetrics::mauc(y_test_class, pred)



save_model_tf(object = model, filepath = "model")

reloaded_model <- load_model_tf("model")
all.equal(predict(model, mnist$test$x), predict(reloaded_model, mnist$test$x))
