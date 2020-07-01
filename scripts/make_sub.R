library(data.table)

sub_0 = fread('cache/final_pred_0.csv')
sub_1 = fread('cache/final_pred_1.csv')
sub_2 = fread('cache/final_pred_2.csv')

# Checks
stopifnot(sum(colnames(sub_0) == colnames(sub_1)) == 29)
stopifnot(sum(colnames(sub_0) == colnames(sub_2)) == 29)
stopifnot(sum(sub_0$id == sub_1$id) == 60980)
stopifnot(sum(sub_0$id == sub_2$id) == 60980)

# Combine
final_sub = sub_0[, .(id)]
for(i in 1:28) {
  col_name = paste0('F', i)
  col_pred = rowMeans(cbind(sub_0[, (col_name), with = F]
                            ,sub_1[, (col_name), with = F]
                            ,sub_2[, (col_name), with = F]
                            ))
  final_sub[, (col_name) := col_pred]
}
# More checks
stopifnot(sum(final_sub$id == sub_0$id) == nrow(final_sub))
stopifnot(nrow(final_sub) == 60980)
stopifnot(sum(colnames(final_sub) == colnames(sub_1)) == 29)

# Save the submission
fwrite(final_sub, 'cache/final_submission.csv')




