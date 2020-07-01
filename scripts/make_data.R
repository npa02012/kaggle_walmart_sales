cat('Loading libraries\n')
library(data.table)
library(dplyr)
library(stringr)

cat('Loading feature-engineering functions')
source('fe_functions.R')

cat('Setting up config variables\n')
PATH_DATA <- '../data/'
i_train_values =  c(seq(0, 1600, 3)) # c(seq(0, 1600, 100))
i_train_values = c(0, 7, 14, 28, 42, 56, i_train_values)
i_train_values = unique(i_train_values) 
FIRST_TEST_DAY = 1914 + 28
FIRST_TRAIN_DATE = FIRST_TEST_DAY - 1


cat('Loading data\n')
d_sales_train <- fread(paste0(PATH_DATA, 'sales_train_evaluation.csv')) 
d_calendar <- fread(paste0(PATH_DATA, 'calendar.csv'))

cat('Making d_snap\n')
d_snap = rbindlist(list(d_calendar[, .(d, state_id = 'CA', snap = snap_CA)]
                        ,d_calendar[, .(d, state_id = 'TX', snap = snap_TX)]
                        ,d_calendar[, .(d, state_id = 'WI', snap = snap_WI)]
))

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Putting Together Tables
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
cat('Making train data\n')
first_loop = TRUE
for(j in c(1:length(i_train_values))) {
  # Show progress
  if(j %% 5 == 0) cat(j/length(i_train_values), '\n')
  i = i_train_values[j]
  # Make X_tmp and Y_tmp
  r = sales_FE(FIRST_TRAIN_DATE - i, T)
  X_tmp = r$X
  y_tmp = r$y
  # Concatenating X_l
  if(first_loop) {
    X_train = X_tmp
    y_train = y_tmp
    first_loop = FALSE
  } else {
    X_train = rbindlist(list(X_train, X_tmp))
    y_train = rbindlist(list(y_train, y_tmp))
  }
}
r = remaining_FE(X_train, y_train)
X_train = r$X
y_train = r$y

# Save data to be used by make_models.R
fwrite(X_train, 'cache/X_train.csv')
fwrite(y_train, 'cache/y_train.csv')

