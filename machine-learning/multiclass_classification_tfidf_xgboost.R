library(xgboost)
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

# prepare data starting from medical note text==================================

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
y <- dat[, y]
n_class <- length(unique(y))

# split X and y into train and test
set.seed(1234)
in_train <- sample(1:nrow(X), round(0.7 * nrow(X)))
in_test <- setdiff(1:nrow(X), in_train) %>%
  sample()  # to shuffle the row numbers
X_train <- X[in_train,]
y_train <- y[in_train] 
X_test <- X[in_test,]
y_test <- y[in_test]


# train model ==================================================================

n_rep <- 20
# placeholder for hyperparamters
param_df = data.frame(max_depth = numeric(n_rep),
                      eta = numeric(n_rep),
                      seed_number = integer(n_rep),
                      best_metric = numeric(n_rep),
                      best_metric_round = integer(n_rep))
metrics_cv <- vector("list", n_rep)

best_metric <- Inf
pb <- progress_bar$new(total = n_rep)
for (i in 1:n_rep) {
  pb$tick()
  param <- list(objective = "multi:softmax",
                num_class = n_class,
                eval_metric = "merror",  # check for metric for multiclass
                max_depth = sample(2:10, 1),
                eta = runif(1, 0.1, 0.5)
  )
  cv_nround = 20
  cv_nfold = 3
  seed_number = sample.int(10000, 1)
  set.seed(seed_number)
  xgb_cv <- xgb.cv(data=X_train, label = y_train, params = param, 
                   nthread=3, nfold=cv_nfold, nrounds=cv_nround,
                   verbose = TRUE, early_stop_round=10, maximize=FALSE)
  
  cv_metrics <- xgb_cv$evaluation_log %>% 
    set_colnames(c("iter", "train_mean", "train_std", "test_mean", "test_std"))
  
  if(as.data.frame(param)[1, "eval_metric"] %in% c("auc")){
    metric = max(cv_metrics[, test_mean])
    metric_round = which.max(cv_metrics[, test_mean])
    if (best_metric < metric) {
      best_metric <- metric
      best_metric_round <- metric_round
      best_seednumber <- seed_number
      best_param <- param
    }
  } else {
    metric = min(cv_metrics[, test_mean])
    metric_round = which.min(cv_metrics[, test_mean])
    if (best_metric > metric) {
      best_metric <- metric
      best_metric_round <- metric_round
      best_seednumber <- seed_number
      best_param <- param
    }
  }
  
  
  param_df[i, ] <- c(as.data.frame(param)[1, "max_depth"],
                     as.data.frame(param)[1, "eta"],
                     seed_number,
                     metric,
                     metric_round)
  metrics_cv[[i]] <- cv_metrics
  
  
}

# help function to plot xgb.cv metrics
plot_cv <- function(rep_id){
  dt <- metrics_cv[[rep_id]]
  ggplot(dt) +
    geom_line(aes(iter, train_mean), color = "blue") +
    geom_line(aes(iter, test_mean), color = "red")
}

plot_cv(3)  # plot cv result of nth repeat

# final training ===============================================================
set.seed(best_seednumber)
xgb <- xgboost(data = X_train, 
               label = y_train, 
               params = best_param,
               nthread = 3, 
               nrounds = best_metric_round)






# evaluate model with test data ================================================
# get model metrics use custum defined function

pred <- predict(xgb, X_test)


cm <- table(y_test, pred)  # confusion matrix
n <- nrow(cm)
cm_dt <- as.data.table(matrix(unlist(cm), ncol = 1)) %>%
  .[, x := rep(0:(n-1), n)] %>%
  .[, y := rep(0:(n-1), each = n)]
classes <- unique(dat$sample_type) %>%
  str_replace(" / ", "\n")

ggplot() + 
  geom_jitter(aes(y_test, pred), color = "blue", size = 1,
              width = 0.15, height = 0.15, alpha = 0.3) +
  geom_text(data = cm_dt, aes(x, y + 0.3, label = V1), color = "red") +
  scale_x_continuous(breaks = 0:6, labels = classes) +
  scale_y_continuous(breaks = 0:6, labels = classes) +
  labs(x = "True Sample Type",
       y = "Predicted Sample Type")



caret::confusionMatrix(as.factor(pred), as.factor(y_test))

# save the model for future prediction =========================================
saveRDS(xgb, file = "trained_models/multiclass_classification_tfidf_xgboost.rda")
multi_xgb <- readRDS("trained_models/multiclass_classification_tfidf_xgboost.rda")
pred_1 <- predict(multi_xgb, X_test)
identical(pred, pred_1)
