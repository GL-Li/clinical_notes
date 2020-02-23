
### svm
The total number of samples in gas-neu-uro is only 599 while the matrix has 1644 columns. Apply svm directly on this matrix produces very poor result, even cannot train, as the R e1071 package `svm()` function is uable to handle constant columns. In the case of tfidf, some columns only have a few non-zero rows. After train-test splitting, the train set has chances to have columns with all values 0. To get around this problem, we use PCA and keep the top principle components. 

### Metrics of each algorithm
Data split: 70% in train and 30% in test. 
The train data is further split to 70% (or 2/3) for training and 30% (or 1/3) for validation to tune hyperparameters. The final model is built with these parameters. 

The test data is used in prediction. The metric is calculated from y_test and y_pred.

Repeat above process 100 time to calculate mean and standard deviation of each metrics, as shown in the table below.

For svm, we keep first 25 principle components, as it performs best that keeping fewer or more principle components.
