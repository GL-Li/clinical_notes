library(xgboost)
source("utilities.R")


# prepare data =================================================================
set.seed(12345)
dat <- read_notes(
  "data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
  duplicate_rm = T,
  cols_keep = "all",
  y_label = TRUE
)

# hyper-parameters tuning ======================================================
# results: optimal paramters:
#   max_depth = 4
#   eta = 0.28
#   nround = 25

tfidf <- tfidf_tm(dat$amazon_me)
# run only if need pca
#tfidf <- prcomp(tfidf)$x
X <- tfidf
y <- dat$y

set.seed(12345)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]

n_rep <- 100
# placeholder for hyperparamters and metrics
param_df = data.frame(max_depth = numeric(n_rep),
                      eta = numeric(n_rep),
                      metrics = numeric(n_rep),
                      nround = integer(n_rep))
metrics_cv <- vector("list", n_rep)

n_class <- length(unique(y_train))
pb <- progress_bar$new(total = n_rep)
for (i in 1:n_rep) {
  pb$tick()
  maxdepth <- sample(2:10, 1)
  eta <- runif(1, 0.1, 0.5)
  
  param <- list(objective = "multi:softmax",
                num_class = n_class,
                eval_metric = "merror",  # check for metric for multiclass
                max_depth = maxdepth,
                eta = eta
  )
  cv_nround = 30
  cv_nfold = 3
  xgb_cv <- xgb.cv(data=X_train, label = y_train, params = param, 
                   nthread=3, nfold=cv_nfold, nrounds=cv_nround,
                   verbose = FALSE, early_stop_round=10, maximize=FALSE)
  
  cv_metrics <- xgb_cv$evaluation_log %>% 
    set_colnames(c("iter", "train_mean", "train_std", "test_mean", "test_std"))
  
  if(as.data.frame(param)[1, "eval_metric"] %in% c("auc")){
    best_metric = max(cv_metrics[, test_mean])
    best_metric_round = which.max(cv_metrics[, test_mean])
  } else {
    best_metric = min(cv_metrics[, test_mean])
    best_metric_round = which.min(cv_metrics[, test_mean])
  }
  
  param_df[i, ] <- c(maxdepth, eta, best_metric, best_metric_round)
  metrics_cv[[i]] <- cv_metrics
}

# plot to find best parameters
plot(param_df$max_depth, param_df$metrics)
plot(param_df$eta, param_df$metrics)
plot(param_df$nround, param_df$metrics)




# one model ====================================================================
param <- list(objective = "multi:softmax",
              num_class = n_class,
              eval_metric = "merror",
              max_depth = 4,
              eta = 0.28
)
xgb <- xgboost(data = X_train, 
               label = y_train, 
               params = param,
               nthread = 3, 
               nrounds = 25)

y_pred <- predict(xgb, X_test)
table(y_test, y_pred)


# repeat 100 times to get average metrics =====================================
xgb_metrics <- function(corpus, pca = FALSE){
  # calculate average accuracy and f1 score out of 100 repeat
  # 
  # Arguments:
  #   corpus: string, "note" or "amazon_me"
  #   pca: bool, if TRUE, process with pca
  #
  # Return:
  #   numeric vector 
  #
  tfidf <- tfidf_tm(dat[, get(corpus)])
  # run only if need pca
  if (pca){
    tfidf <- prcomp(tfidf)$x[, 1:25]
  }
  X <- tfidf
  y <- dat$y
  
  n_rep <- 100
  df_acc_f1 <- data.frame(
    acc = numeric(n_rep),
    f1_gas = numeric(n_rep),
    f1_neu = numeric(n_rep),
    f1_uro = numeric(n_rep)
  )
  set.seed(6789)
  in_trains <- caret::createDataPartition(y, times = n_rep, p = 0.7)
  
  for (i in 1:100){
    cat(i)
    in_train <- in_trains[[i]]
    X_train <- X[in_train, ]
    y_train <- y[in_train]
    X_test <- X[-in_train,]
    y_test <- y[-in_train]
    
    mdl <- xgboost(data = X_train, 
                   label = y_train, 
                   params = list(
                     objective = "multi:softmax",
                     num_class = 3,
                     eval_metric = "merror",
                     max_depth = 4,
                     eta = 0.28
                   ),
                   nthread = 3, 
                   nrounds = 25,
                   verbose = FALSE)
    y_pred <- predict(mdl, X_test)
    
    tb <- table(y_test, y_pred)
    acc <- sum(diag(tb)) / length(y_test)
    
    f1_score <- function(tb, k){
      recall <- diag(tb)[k] / sum(y_test == k - 1)
      precision <- diag(tb)[k] / sum(y_pred == k - 1)
      f1 <- 2 * (recall * precision) / (recall + precision)
    }
    
    f1_gas <- f1_score(tb, 1)
    f1_neu <- f1_score(tb, 2)
    f1_uro <- f1_score(tb, 3)
    
    # assert correct calculation
    stopifnot(acc <= 1 & f1_gas <= 1 & f1_neu <= 1 & f1_uro <= 1)
    stopifnot(acc >= 0 & f1_gas >= 0 & f1_neu >= 0 & f1_uro >= 0)
    
    df_acc_f1[i, ] <- c(acc, f1_gas, f1_neu, f1_uro)
  }
  
  return(df_acc_f1)
}

# mean and standard deviation
df_acc_f1 <- xgb_metrics("note")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- xgb_metrics("amazon_me")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- xgb_metrics("note", pca = TRUE)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- xgb_metrics("amazon_me", pca = TRUE)
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

