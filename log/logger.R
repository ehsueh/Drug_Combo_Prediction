# master.R should have set the working directory

write_to_master_log <- function(file, features, dropout, hidden, epochs, train_r2, train_mse, xval_r2, xval_mse){
  new_row <- paste(file, features, dropout, hidden, epochs, train_r2, train_mse, xval_r2, xval_mse, sep = ",")
  write.csv(new_row, "log/master_log.csv", append = TRUE)
}

write_to_log_file <- function(msg){
  write(msg, LOG_PATH, append = TRUE)
}