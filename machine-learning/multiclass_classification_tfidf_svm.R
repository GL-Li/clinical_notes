# Only use well-defined medical specialties which have more than 150 samples 
# removing duplicates

library(e1071)
library(progress)
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
  duplicate_rm = T,
  specialties = specialties,
  cols_keep = cols,
  id = TRUE,
  y_label = TRUE
)
tfidf <- tfidf_tm(dat$note)
y <- as.factor(dat$y)  # svm requires y to be factor


# find best n_pca ==============================================================
# result: keep first 20 - 50 principle components have the best accuracy.
# We will use n_pca = 25 considering accuracy and speed.
# run only if need pca
X <- prcomp(tfidf)$x

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
    
    plot(pc_acc$n_pc, pc_acc$acc, type = "p", xlim = c(0, max(n_pcas)))
  }
  
  # plot(pc_acc$n_pc, pc_acc$acc, type = "p")
}


# # find the best n_pca
n_pcas <- c(2:50, 2 * (26:50), 5 * (21:45))
plot_n_pca(n_pcas, X_train, y_train, 10)

# one model tfidf ================================================================
# results: poor recall and precision

X <- tfidf

set.seed(11111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]

mdl <- svm(X_train, y_train)
y_pred <- predict(mdl, X_test)

classes_x <- c(
  "Gastroenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
classes_y <- c(
  "Gastro-\nenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)

ggplot_multiclass_svm_tfidf_recall <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y, type = "recall")
ggplot_multiclass_svm_tfidf_precision <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y, type = "precision")

accuracy_svm_tfidf <- accuracy(y_test, y_pred)


# one model pca ================================================================
# visually pick 25 as the best n_pca to train model
# results: exellent recall and precision
X <- prcomp(tfidf)$x[, 1:25]

set.seed(11111)
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[in_train,]
X_test <- X[-in_train,]

y_train <- y[in_train]
y_test <- y[-in_train]

mdl <- svm(X_train, y_train)
y_pred <- predict(mdl, X_test)

classes_x <- c(
  "Gastroenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
classes_y <- c(
  "Gastro-\nenterology", "Obstetrics\nGynecology",  "Cardiovascular\nPulmonary", 
  "Neurology", "Urology", "Orthopedic"
)
ggplot_multiclass_svm_pca_recall <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y)
ggplot_multiclass_svm_pca_precision <- plot_confusion_matrix(y_test, y_pred, classes_x, classes_y, type = "precision")

accuracy_svm_pca <- accuracy(y_test, y_pred)
model_svm_pca <- mdl


# save for shiny ===============================================================
save(ggplot_multiclass_svm_pca_recall, ggplot_multiclass_svm_pca_precision,
     ggplot_multiclass_svm_tfidf_recall, ggplot_multiclass_svm_tfidf_precision,
     accuracy_svm_tfidf, accuracy_svm_pca,
     file = "shiny-apps/RData/ggplot_multiclass_svm.RData")

saveRDS(model_svm_pca, file = "shiny-apps/trained_models/model_svm_pca.rda")

# # to load model
# loaded_model <- readRDS("shiny-apps/trained_models/model_svm_pca.rda")