library(xgboost)
library(data.table)
library(magrittr)
#library(caret)
library(progress)
library(stringr)
source("utilities.R")

# prepare data =================================================================

dat <- fread("data/mtsample_gastroenterology_neurology.csv") %>%
  .[, note := str_replace_all(note, "\\.", "\\. ")] %>%
  .[, y := as.integer(factor(specialty)) - 1]
X <- tfidf_tm(dat$note)
y <- dat$y

set.seed(1234)
in_train <- sample(1:nrow(X), round(0.7 * nrow(X)))
in_test <- setdiff(1:nrow(X), in_train) %>%
  sample()
X_train <- X[in_train,]
y_train <- y[in_train]
X_test <- X[in_test,]
y_test <- y[in_test]

# parameter tuning =============================================================
n_rep <- 100
param_df = data.frame(max_depth = numeric(n_rep),
                      eta = numeric(n_rep),
                      seed_number = integer(n_rep),
                      best_metric = numeric(n_rep),
                      best_metric_round = integer(n_rep))
metrics_cv <- vector("list", n_rep)

best_metric <- Inf  # when metric is the smaller the better
pb <- progress_bar$new(total = n_rep)

for (i in 1:n_rep) {
    pb$tick()
    param <- list(objective = "binary:logistic",
                  eval_metric = "error",
                  max_depth = sample(2:10, 1),
                  eta = runif(1, 0.1, 0.5)
    )
    cv_nround = 100
    cv_nfold = 3  # small sample size
    seed_number = sample.int(10000, 1)
    set.seed(seed_number)
    xgb_cv <- xgb.cv(data=X_train, label = y_train, params = param, 
                     nthread=3, nfold=cv_nfold, nrounds=cv_nround,
                     verbose = FALSE, maximize=FALSE)
    
    cv_metrics <- xgb_cv$evaluation_log %>% 
        set_colnames(c("iter", "train_mean", "train_std", "test_mean", "test_std"))
      
    # When eval_metric is the larger the better
    if(as.data.frame(param)[1, "eval_metric"] %in% c("auc", "map")){
        metric <- max(cv_metrics[, test_mean])
        metric_round <- which.max(cv_metrics[, test_mean])
        if (metric > best_metric) {
            best_metric <- metric
            best_metric_round <- metric_round
            best_seednumber <- seed_number
            best_param <- param
        }
    # when eval_matric is the smaller the better
    } else {
        metric <- min(cv_metrics[, test_mean])
        metric_round <- which.min(cv_metrics[, test_mean])
        if (metric < best_metric) {
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



# final training ===============================================================
set.seed(best_seednumber)
xgb <- xgboost(data = X_train, 
               label = y_train, 
               params = best_param,
               nthread = 3, 
               nrounds = best_metric_round)


pred <- predict(xgb, X_test)
pred_class <- as.integer(pred > 0.5)
metrics_binary(y_test, pred)


saveRDS(xgb, file = "trained_models/binary_classification_tfidf_xgboost.rda")
