cat('Loading libraries\n')
library(lightgbm)
library(data.table)

cat('Loading feature-engineering functions')
source('fe_functions.R')

cat('Setting up config variables\n')
PATH_DATA <- '../data/'
FIRST_TEST_DAY = 1914 + 28
FIRST_TRAIN_DATE = FIRST_TEST_DAY - 1

for(SEED in 0:2) {
  cat('Loading data\n')
  d_sales_train <- fread(paste0(PATH_DATA, 'sales_train_evaluation.csv')) 
  d_calendar <- fread(paste0(PATH_DATA, 'calendar.csv'))
  
  cat('Making d_snap\n')
  d_snap = rbindlist(list(d_calendar[, .(d, state_id = 'CA', snap = snap_CA)]
                          ,d_calendar[, .(d, state_id = 'TX', snap = snap_TX)]
                          ,d_calendar[, .(d, state_id = 'WI', snap = snap_WI)]
  ))
  
  cat('Loading training data\n')
  X_train = fread('cache/X_train.csv')
  y_train = fread('cache/y_train.csv')
  
  bst = lgb.load('cache/bst_0')
  
  cat('Making test predictions\n')
  for(i in 0:27) {
    cat(round(i/27, 4), '\n')
    # Make test data
    r = sales_FE(FIRST_TEST_DAY + i, F)
    X_test = remaining_FE(r$X)$X
    # Make prediction
    tmp_preds = bst$predict(as.matrix(X_test[, -c('id'), with=F]), num_iteration = bst$best_iter)
    # Attach predictions to train table
    d_tmp = data.table(id = X_test[, id], tmp = tmp_preds)
    colnames(d_tmp) = c('id', paste0('d_', FIRST_TEST_DAY + i))
    d_sales_train = left_join(d_sales_train, d_tmp, by = 'id') %>% as.data.table
    # Initialize prediction table if first loop
    if(i == 0) {
      test_pred = X_test[, .(id)] 
    }
    # Attach predictions to prediction table
    y_col = paste0('F', i+1)
    test_pred[, (y_col) := tmp_preds]  
  }
  
  cat('Making validation predictions\n')
  FIRST_VAL_DAY = FIRST_TEST_DAY - 28 
  cols_to_delete = paste0('d_', FIRST_VAL_DAY:(FIRST_VAL_DAY + 100))
  cols_to_delete = cols_to_delete[cols_to_delete %in% colnames(d_sales_train)]
  d_sales_train = d_sales_train[, -c(cols_to_delete), with = F]
  for(i in 0:27) {
    cat(round(i/27, 4), '\n')
    # Make test data
    r = sales_FE(FIRST_VAL_DAY + i, F)
    X_test = remaining_FE(r$X)$X
    # Make prediction
    tmp_preds = bst$predict(as.matrix(X_test[, -c('id'), with=F]), num_iteration = bst$best_iter)
    # Attach predictions to train table
    d_tmp = data.table(id = X_test[, id], tmp = tmp_preds)
    colnames(d_tmp) = c('id', paste0('d_', FIRST_VAL_DAY + i))
    d_sales_train = left_join(d_sales_train, d_tmp, by = 'id') %>% as.data.table
    # Initialize prediction table if first loop
    if(i == 0) {
      val_pred = X_test[, .(id)] 
    }
    # Attach predictions to prediction table
    y_col = paste0('F', i+1)
    val_pred[, (y_col) := tmp_preds]  
  }
  
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Cleaning and Submitting Predictions
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  
  cat('Preparing predictions\n')
  prepare_preds <- function(preds_val, preds_final, d_prices) {
    cat('Attaching evaluation rows\n')
    preds_val[, id := str_replace(id, '_evaluation', '_validation')]
    final_pred = rbindlist(list(preds_val, preds_final))
    col_order = c('id', paste0('F', 1:28))
    final_pred = final_pred[, c(col_order), with = FALSE]
    
    cat('Replacing negative predictions with 0\n')
    for(c in colnames(test_pred)) {
      if(c == 'id') next
      tmp = sum(final_pred[, c, with=F] >= 0)
      if(tmp != 60980) {
        cat(sum(final_pred < 0), ' predictions were negative.\n')
        final_pred[final_pred < 0] = 0
      }
    }
    
    cat('Checking predictions format\n')
    d_sample = fread(paste0(PATH_DATA, 'sample_submission.csv'))
    d_sample = d_sample[order(id)]
    tmp = copy(final_pred)
    tmp = tmp[order(id)]
    stopifnot(sum(tmp$id == d_sample$id) == 60980)
    
    # Return
    return(final_pred)
  }
  
  final_pred = prepare_preds(val_pred, test_pred, d_prices)
  
  cat('Saving submission\n')
  fwrite(final_pred, paste0('cache/final_pred_', SEED, '.csv'))
  
}

