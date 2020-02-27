library(text2vec)
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
y <- dat$y  
notes <- dat$note


# train-test split =============================================================
# check functions in utilities.R under text2vec
in_train <- caret::createDataPartition(y, p = 0.7, list = FALSE)

train <- notes[in_train]
test <- notes[-in_train]
train_y <- y[in_train]
test_y <- y[-in_train]

train_vocab <- get_vocab(train) 
train_vocab <- train_vocab[str_detect(train_vocab$term, "^[a-z]{2,}$"),]

train_vectorizer <- get_vectorizer(train_vocab)
train_dtm <- get_dtm(train, train_vectorizer)
test_dtm <- get_dtm(test, train_vectorizer)

tfidf_model <- fit_tfidf(train_dtm)
train_tfidf <- transform_tfidf(train_dtm, tfidf_model)
test_tfidf <- transform_tfidf(test_dtm, tfidf_model)

pca_model <- fit_pca(train_tfidf)
train_pca <- predict(pca_model, train_tfidf)
test_pca <- predict(pca_model, test_tfidf)


# save data for multiclass classification and model deployment =================
save(train_tfidf, test_tfidf,
     train_pca, test_pca,
     train_y, test_y,
     train_vectorizer, tfidf_model, pca_model,
     file = "multiclass_classification_train_test_tfidf_pca_models.RData")

save(train_vectorizer, tfidf_model, pca_model,
     file = "shiny-apps/RData/multiclass_classification_tfidf_pca_models.RData")
