cat('Loading libraries\n')
library(lightgbm)
library(data.table)

cat('Setting up config variables\n')
learning_rate = .15

cat('Setting up config variables\n')
FIRST_TEST_DAY = 1914 + 28
FIRST_TRAIN_DATE = FIRST_TEST_DAY - 1


for(SEED in 0:2) {
  set.seed(SEED)
  
  cat('Loading training data\n')
  X_train = fread('cache/X_train.csv')
  y_train = fread('cache/y_train.csv')
  # Keep 1914 --> 1941 as validation
  idxs = which(X_train$pred_day_num >= 1914)
  X_train = X_train[-idxs]
  y_train = y_train[-idxs]  
  
  cat('Using random 10% sample from X_train for validation\n')
  val_rows = sample(1:nrow(X_train), .1 * nrow(X_train))
  X_val = X_train[val_rows]
  X_train = X_train[-val_rows]
  y_val = y_train[val_rows]
  y_train = y_train[-val_rows]
  
  
  cat("Setting LGBM Parameters\n")
  MAX_ROUNDS = 2000
  params <- list(num_leaves = 120
                 ,objective = "poisson"
                 ,metric = "rmse"
                 ,min_data_in_leaf = 30
                 ,learning_rate = learning_rate
                 ,feature_fraction = .8
                 ,bagging_fraction = .8
                 ,bagging_freq = 2
                 ,num_threads = 12
  )
  
  cat('Building models\n')
  # Setup
  y_col = "F1"
  # Make tables
  dtrain <- lgb.Dataset(as.matrix(X_train[, -c('id'), with=F])
                        ,label = as.matrix(y_train[, y_col, with=F]))
  dval <- lgb.Dataset(as.matrix(X_val[, -c('id'), with=F])
                      ,label = as.matrix(y_val[, y_col, with=F])
                      ,reference=dtrain)
  # Train
  bst <- lgb.train(params = params
                   ,data = dtrain
                   ,nrounds = MAX_ROUNDS
                   ,valids = list(test=dval)
                   ,early_stopping_rounds = 10
                   ,verbose = 1
                   ,eval_freq = 15
  )
  lgb.save(bst, paste0('cache/bst_', SEED))  
}
 


