library(data.table)
library(dplyr)
library(stringr)

sales_FE <- function(pred_day_num, is_train) {
  
  # Initialize X
  X <- d_sales_train[, .(id, cat_id, state_id)]
  X[, pred_day_num := pred_day_num]
  
  
  
  # Get lags
  lags = c(7, 28)
  for(l in lags) {
    cols = paste0('d_', pred_day_num - l)
    col_name = paste0('lag_', l)
    X[, (col_name) := d_sales_train[, cols, with = F]]
  }
  
  # Get lagged rolling means
  lags = c(7, 28)
  windows = c(7, 28)
  for(l in lags) {
    for(w in windows) {
      cols = paste0('d_', (pred_day_num - l - w + 1):(pred_day_num - l))
      col_name = paste0('rmean_', l, '_', w)
      X[, (col_name) := d_sales_train[, rowMeans(.SD), .SDcols = cols]]
    }
  }
  
  # Put together target variable if a training set
  if(is_train) {
    y_day = paste0('d_', pred_day_num)
    y = d_sales_train[, c('id', y_day), with=F]
    colnames(y) = c('id', 'F1')
    y[is.na(y)] = 0
  } else {
    y = NULL
  }
  return(list(X=X, y=y))
}

remaining_FE <- function(X, y=NA) {
  
  # Attach a row_order column (deleted at the end)
  X[, row_order := 1:nrow(X)]
  if(is.data.table(y)) {
    y[, row_order := 1:nrow(X)]
  }
  
  # Attach pred_day_string (temporary)
  X[, pred_day_string := paste0('d_', pred_day_num)]
  
  # Attach calendar information
  X = left_join(X, d_calendar[, .(d, wm_yr_wk, wday, month, year)]
                ,by = c('pred_day_string' = 'd')) %>% as.data.table
  
  # Add previous event types
  d_calendar_tmp = copy(d_calendar[, .(d, event_type_1)])
  d_calendar_tmp[event_type_1 == '', event_type_1_num := 0]
  d_calendar_tmp[event_type_1 == 'Sporting', event_type_1_num := 1]
  d_calendar_tmp[event_type_1 == 'Cultural', event_type_1_num := 2]
  d_calendar_tmp[event_type_1 == 'National', event_type_1_num := 3]
  d_calendar_tmp[event_type_1 == 'Religious', event_type_1_num := 4]
  d_calendar_tmp[, event_type_1 := NULL]
  for(i in -3:0) {
    X[, tmp_d := paste0('d_', pred_day_num + i)]
    X = left_join(X, d_calendar_tmp, by = c('tmp_d' = 'd')) %>% as.data.table
    if(i < 0) {
      new_col = paste0('event_type_1_m', abs(i))
    } else {
      new_col = paste0('event_type_1_p', i)
    }
    X[, (new_col) := event_type_1_num]
    X[, event_type_1_num := NULL]
    X[, tmp_d := NULL]
  }
  
  
  # Attach SNAP info for previous days
  for(i in -3:0) {
    X[, tmp_d := paste0('d_', pred_day_num + i)]
    X = left_join(X, d_snap, by = c('tmp_d' = 'd'
                                    ,'state_id' = 'state_id')) %>% as.data.table
    if(i < 0) {
      new_col = paste0('snp_m', abs(i))
    } else {
      new_col = paste0('snp_p', i)
    }
    X[, (new_col) := snap]
    X[, snap := NULL]
    X[, tmp_d := NULL]
  }
  
  # Encode state_id and delete
  X[state_id == 'CA', state_num := 0]
  X[state_id == 'TX', state_num := 1]
  X[state_id == 'WI', state_num := 2]
  X[, state_id := NULL]
  
  # Encode cat_id and delete
  X[cat_id == 'FOODS', cat_num := 0]
  X[cat_id == 'HOBBIES', cat_num := 1]
  X[cat_id == 'HOUSEHOLD', cat_num := 2]
  X[, cat_id := NULL]
  
  # Delete pred_day_string
  X[, pred_day_string := NULL]
  
  # Reorder X and y
  X = X[order(row_order)]
  X[, row_order := NULL]
  if(is.data.table(y)) {
    y = y[order(row_order)] 
    y[, row_order := NULL]
  }
  
  # Return
  return(list(X=X, y=y))
}

