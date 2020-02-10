library(xgboost)
library(data.table)
library(magrittr)
library(caret)
library(progress)

# prepare data
load("./shiny-apps/RData/clustering.RData")
X <- tfidf_norm
set.seed(1234)
in_train <- sample(1:nrow(X), round(0.5 * nrow(X)))
in_validation <- 
in_test <- setdiff(1:nrow(X), in_train)
X_train <- X[in_train,]
y_train <- target[in_train] - 1
X_test <- X[in_test,]
y_test <- target[in_test] - 1

# parameter tuning
n_rep <- 20
param_df = data.frame(max_depth = numeric(n_rep),
                    eta = numeric(n_rep),
                    seed_number = integer(n_rep),
                    best_metric = numeric(n_rep),
                    best_metric_round = integer(n_rep))
metrics_cv <- vector("list", n_rep)

best_metric <- 0
pb <- progress_bar$new(total = n_rep)
for (i in 1:n_rep) {
    pb$tick()
    param <- list(objective = "binary:logistic",
                  eval_metric = "auc",
                  max_depth = sample(2:10, 1),
                  eta = runif(1, 0.1, 0.5)
    )
    cv_nround = 100
    cv_nfold = 3
    seed_number = sample.int(10000, 1)
    set.seed(seed_number)
    xgb_cv <- xgb.cv(data=X_train, label = y_train, params = param, 
                     nthread=3, nfold=cv_nfold, nrounds=cv_nround,
                     verbose = FALSE, early_stop_round=10, maximize=FALSE)
    
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

# final training ===============================================================
set.seed(best_seednumber)
xgb <- xgboost(data = X_train, 
               label = y_train, 
               params = best_param,
               nthread = 3, 
               nrounds = best_metric_round)


pred <- predict(xgb, X_test)
pred_class <- as.integer(pred > 0.5)
confusionMatrix(as.factor(pred_class), as.factor(y_test))
ModelMetrics::auc(y_test, pred)
