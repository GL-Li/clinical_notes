library(e1071)
library(progress)
source("utilities.R")


# prepare data =================================================================
# for the 3 class, we use the same data as those used in clustering
load("shiny-apps/RData/pca_note_amazon_gas_neu_uro.RData")


# split train and test, also used in xgb and nn
# the data are already randomized, so no need to sample again

X <- pca_note$x
y <- as.factor(dat_gas_neu_uro$y)

in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[in_train,]
y_train <- y[in_train]
X_test <- X[-in_train,]
y_test <- y[-in_train]


# use tfidf_note ==============================================================
# svm reject constant columns. As tfidf is sparse, the train data often contains
# columns with only value 0. Use pca to solve the problem

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


plot_n_pca <- function(n_pcas, n_split = 10){
  # Plot accuracy ~ n_pca to find best n_pca values
  #
  # n_pcas: int, vector of number of principle components to keep
  # n_split: times of repearing train-val split to get average accuracy
  
  n <- length(n_pcas)
  pc_acc <- data.frame(n_pc = rep(0, n), acc = rep(0, n))
  
  pb <- progress_bar$new()
  for (i in 1:n) {
    pb$tick()
    n_pc <- points[i]
    m <- svm_pca(n_pc, X_train, y_train, n_split)
    pc_acc[i, ] <- m
  }
  
  plot(pc_acc$n_pc, pc_acc$acc, type = "p")
}

points <- c(2:50, 2 * (26:50), 5 * (21:45))

par(mfrow = c(1, 2))
plot_n_pca(points, 1)
plot_n_pca(points, 10)

# visually pick 40 as the best n_pca to train model
mdl <- svm(X_train[, 1:40], y_train)
y_pred <- predict(mdl, X_test[, 1:40])
table(truth = y_test, predict = y_pred)



plot_pc1_pc2(X_test, color = y_test)
plot_pc1_pc2(X_test, color = y_pred)
plot_pc1_pc2(X_test, color = y_test == y_pred, color_map = c("orange", "gray"))

classes_x <- c("Gastronenterology", "Neurology", "Urology")
classes_y <- c("Gastro-\nenterology", "Neurology", "Urology")
plot_confusion_matrix(y_test, y_pred, classes_x, classes_y)

