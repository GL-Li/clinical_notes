library(tensorflow)
library(keras)
library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)

get_metrics <- function(y_true, y_pred, cutoff = 0.5){
  y_pred_class <- round(y_pred)
  auc <- ModelMetrics::auc(y_true, y_pred)
  f1 <- ModelMetrics::f1Score(y_true, y_pred, cutoff)
  sensitivity <- ModelMetrics::sensitivity(y_true, y_pred, cutoff)
  specificity <- ModelMetrics::specificity(y_true, y_pred, cutoff)
  
  cat("confusion matrix:\n       y_true\n")
  print(ModelMetrics::confusionMatrix(y_true, y_pred))
  cat("   \n")
  
  return(c(auc = auc, f1 = f1, sensitivity = sensitivity, specificity = specificity))
}

# prepare data starting from medical note text
dat <- fread("data/mtsamples_multi_class.csv") %>%
  .[, medical_note := str_replace_all(medical_note, "\\.", "\\. ")] %>%
  .[, y := as.integer(factor(sample_type)) - 1]

notes <- dat[, medical_note]
# initialize tokenizer specifing maximum words
max_words <- 3000
tk <- text_tokenizer(num_words = max_words)
# update tk in place with a vector or list of documents
fit_text_tokenizer(tk, notes)
# convert the documents into a list of sequence
X <- texts_to_sequences(tk, notes)
# examine sequence length, the longest is 2471, mean 430
len <- sapply(X, function(x) length(x))
summary(len)
# pad the sequence to get a matrix
seq_length <- 500
X <- pad_sequences(X, seq_length)
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
  layer_embedding(input_dim = max_words, output_dim = 32, 
                  input_length = seq_length) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
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
    verbose = 2
)

evaluate(model, X_test, y_test, verbose = 0)


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
# percent
cm_pct <- cm / rowSums(cm) 
pct_dt <-  as.data.table(matrix(unlist(cm_pct), ncol = 1)) %>%
  .[, x := rep(0:(n-1), n)] %>%
  .[, y := rep(0:(n-1), each = n)]

# count
cm_dt <- as.data.table(matrix(unlist(cm), ncol = 1)) %>%
  .[, x := rep(0:(n-1), n)] %>%
  .[, y := rep(0:(n-1), each = n)]
classes <- unique(dat$sample_type) %>%
  str_replace(" / ", "\n")

main_plot <- ggplot() + 
  geom_jitter(aes(y_test_class, pred_class), color = "blue", size = 1,
              width = 0.1, height = 0.1, alpha = 0.3) +
  geom_text(data = cm_dt, aes(x - 0.05, y + 0.3, label = V1), hjust = 1,
            color = "red") +
  geom_text(data = pct_dt, 
            aes(x + 0.05, y + 0.3, label = paste0(round(100 * V1, 1), "%")), 
            color = "purple", hjust = 0) +
  scale_x_continuous(breaks = 0:6, labels = classes) +
  scale_y_continuous(breaks = 0:6, labels = classes) +
  labs(x = "True Sample Type",
       y = "Predicted Sample Type")

grobs <- grobTree(
  gp = gpar(fontsize = 12, fontface = "bold"), 
  textGrob(label = "    Number", 
           name = "title1",
           x = unit(0.2, "lines"), 
           y = unit(1.4, "lines"), 
           hjust = 0, 
           vjust = 1, 
           gp = gpar(col = "red")),
  textGrob(label = " and ", 
           name = "title2",
           x = grobWidth("title1") + unit(0.2, "lines"),
           y = unit(1.4, "lines"),
           hjust = 0, 
           vjust = 1),
  textGrob(label = "Percentage", 
           name = "title3",
           x = grobWidth("title1") + grobWidth("title2") + unit(0.2, "lines"),
           y = unit(1.4, "lines"),
           gp = gpar(col = "purple"),
           hjust = 0, 
           vjust = 1),
  textGrob(label = " of True Sample Types Being Predicted as Other Types",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + unit(0.2, "lines"),
           y = unit(1.4, "lines"),
           hjust = 0, 
           vjust = 1)
)

gg <- arrangeGrob(main_plot, top=grobs, padding = unit(2.6, "line"))

grid.arrange(gg)



caret::confusionMatrix(as.factor(pred_class), as.factor(y_test_class))
ModelMetrics::mauc(y_test_class, pred)



# save tensorflow model, different from traditional ML mode ====================
save_model_tf(object = model, filepath = "model")

reloaded_model <- load_model_tf("model")
all.equal(predict(model, mnist$test$x), predict(reloaded_model, mnist$test$x))
