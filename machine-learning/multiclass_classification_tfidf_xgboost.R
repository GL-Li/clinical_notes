library(xgboost)
source("utilities.R")

# prepare data =================================================================
specialties <- c(
  "Gastroenterology", "Obstetrics / Gynecology",  "Cardiovascular / Pulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
cols <- c("specialty", "note")
set.seed(1234)
dat <- read_notes(
  "data/mtsamples_multi_class.csv", 
  duplicate_rm = TRUE,
  specialties = specialties,
  cols_keep = cols,
  id = TRUE,
  y_label = TRUE
)
tfidf <- tfidf_tm(dat$note)
y <- dat$y
n_class <- length(unique(y))



# hyper-parameters tuning ======================================================
# results: optimal paramters for both tfidf and pca25:
#   max_depth = 5
#   eta = 0.4
#   nround = 25

# run only if need pca
tfidf <- prcomp(tfidf)$x[, 1:25]
X <- tfidf

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
                   verbose = TRUE, early_stop_round=10, maximize=FALSE)
  
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




# one model: tfidf =============================================================
X <- tfidf

set.seed(11111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]
param <- list(objective = "multi:softmax",
              num_class = n_class,
              eval_metric = "merror",
              max_depth = 5,
              eta = 0.4
)
xgb <- xgboost(data = X_train, 
               label = y_train, 
               params = param,
               nthread = 3, 
               nrounds = 25)

y_pred <- predict(xgb, X_test)
table(y_test, y_pred)

classes_x <- c(
  "Gastroenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
classes_y <- c(
  "Gastro-\nenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
ggplot_multiclass_xgb_tfidf_recall<- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y)
ggplot_multiclass_xgb_tfidf_precision <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y, type = "precision")

accuracy_xgb_tfidf <- accuracy(y_test, y_pred)




# one model: pca   =============================================================
X <- prcomp(tfidf)$x[, 1:25]

set.seed(11111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]
param <- list(objective = "multi:softmax",
              num_class = n_class,
              eval_metric = "merror",
              max_depth = 5,
              eta = 0.4
)
xgb <- xgboost(data = X_train, 
               label = y_train, 
               params = param,
               nthread = 3, 
               nrounds = 25)

y_pred <- predict(xgb, X_test)
table(y_test, y_pred)

classes_x <- c(
  "Gastroenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
classes_y <- c(
  "Gastro-\nenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
ggplot_multiclass_xgb_pca_recall <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y)
ggplot_multiclass_xgb_pca_precision <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y, type = "precision")

accuracy_xgb_pca <- accuracy(y_test, y_pred)




# save for shiny
save(ggplot_multiclass_xgb_pca_recall, ggplot_multiclass_xgb_pca_precision,
     ggplot_multiclass_xgb_tfidf_recall, ggplot_multiclass_xgb_tfidf_precision,
     accuracy_xgb_tfidf, accuracy_xgb_pca,
     file = "shiny-apps/RData/ggplot_multiclass_xgb.RData")

