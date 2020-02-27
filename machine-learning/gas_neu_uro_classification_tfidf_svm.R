# Use support vector machine to train model 
# three specialties: Gastroenterology, Neurology, and Urology
# Use clinical notes or Amazon Comprehend Medical entities


library(e1071)
library(progress)
source("utilities.R")


# prepare data =================================================================
set.seed(12345)
dat <- read_notes(
  "data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv",
  duplicate_rm = T,
  cols_keep = "all",
  y_label = TRUE
)
y <- as.factor(dat$y)  # svm requires y to be factor




# find best n_pca ==============================================================
# result: keep first 20 - 50 principle components have the best accuracy.
# We will use n_pca = 25 considering accuracy and speed.
# choose from note or amazon_me
tfidf <- tfidf_tm(dat$amazon_me, sparsity = 0.95)
# run only if need pca
tfidf <- prcomp(tfidf)$x
X <- tfidf

set.seed(1111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]


svm_pca <- function(n_pc, X_train, y_train, n_split){
  # Calculate the accuracy for number of principle components used in SVM
  #
  # n_pc: int, number of principle components to keep
  # X_train, y_train: trainig data
  # n_split: int, times of repeating train-validation split
  
  # split train and validation data
  X_train <- X_train[, 1:n_pc]
  
  # create 10 split index
  intrains <- caret::createDataPartition(
    y_train, times = n_split, p = 0.6, list = FALSE
  )
  
  acc <- 0
  for (i in 1:n_split){
    intrain <- intrains[, i]
    Xtrain <- X_train[intrain,]
    ytrain <- y_train[intrain]
    Xval <- X_train[-intrain,]
    yval <- y_train[-intrain]
    mdl <- svm(Xtrain, ytrain)
    ypred <- predict(mdl, Xval)
    tb <- table(truth = yval, predict = ypred)
    accuracy <- sum(diag(tb)) / length(yval)
    acc <- acc + accuracy / n_split
  }
  
  return(c(n_pc = n_pc, acc = accuracy))
}


plot_n_pca <- function(n_pcas, X_train, y_train, n_split = 100){
  # Plot accuracy ~ n_pca to find best n_pca values
  #
  # n_pcas: int, vector of number of principle components to keep
  # n_split: times of repearing train-val split to get average accuracy
  
  n <- length(n_pcas)
  pc_acc <- data.frame(n_pc = rep(0, n), acc = rep(0, n))
  
  pb <- progress_bar$new()
  for (i in 1:n) {
    pb$tick()
    n_pc <- n_pcas[i]
    m <- svm_pca(n_pc, X_train, y_train, n_split)
    pc_acc[i, ] <- m
  }
  
  plot(pc_acc$n_pc, pc_acc$acc, type = "p")
}


# # find the best n_pca
n_pcas <- c(2:50, 2 * (26:50), 5 * (21:45))
plot_n_pca(n_pcas, X_train, y_train, 10)



# one model pca====
tfidf <- tfidf_tm(dat$amazon_me, sparsity = 0.992)
# run only if need pca
X <- prcomp(tfidf)$x

set.seed(1111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]

# visually pick 25 as the best n_pca to train model
mdl <- svm(X_train[, 1:25], y_train)
y_pred <- predict(mdl, X_test[, 1:25])
table(truth = y_test, predict = y_pred)



plot_pc1_pc2(X_test, color = y_test)
plot_pc1_pc2(X_test, color = y_pred)
plot_pc1_pc2(X_test, color = y_test == y_pred, color_map = c("black", "gray"))

classes_x <- c("Gastronenterology", "Neurology", "Urology")
classes_y <- c("Gastro-\nenterology", "Neurology", "Urology")
plot_confusion_matrix(y_test, y_pred, classes_x, classes_y)


# one model tfidf ====
# result: svm does not work in this case
tfidf <- tfidf_tm(dat$amazon_me, sparsity = 0.992)
X <- tfidf

set.seed(1111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]

mdl <- svm(X_train, y_train)
y_pred <- predict(mdl, X_test)
table(truth = y_test, predict = y_pred)

classes_x <- c("Gastronenterology", "Neurology", "Urology")
classes_y <- c("Gastro-\nenterology", "Neurology", "Urology")
plot_confusion_matrix(y_test, y_pred, classes_x, classes_y, type = "precision")


# average f1 score and accuracy ==================
# for convenience, copy data praparation here
svm_metrics <- function(corpus){
  # Calculate accuracy and f1 score
  #
  # corpus: string, "note" or "amazon_me"
  
  # choose from note or amazon_me
  tfidf <- tfidf_tm(dat[, get(corpus)])
  # run only if need pca
  X <- prcomp(tfidf)$x[, 1:25]
  y <- as.factor(dat$y)  # svm requires y to be factor
  
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
    
    mdl <- svm(X_train, y_train)
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
    
    df_acc_f1[i, ] <- c(acc, f1_gas, f1_neu, f1_uro)
  }
  
  return(df_acc_f1)
}
# mean and standard deviation of accuracy and f1 score
df_acc_f1 <- svm_metrics("note")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

df_acc_f1 <- svm_metrics("amazon_me")
sapply(df_acc_f1, mean)
sapply(df_acc_f1, sd)

