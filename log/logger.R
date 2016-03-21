# master.R should have set the working directory

write_to_master_log <- function(id, train_r2, xval_r2, date, log_file_path){
  new_row <- paste(id, train_r2, xval_r2, date, log_file_path, sep = ",")
  write(new_row, "/home/zack/Drug_Combo_Prediction/log/master_log.csv", append = TRUE)
}

write_to_log_file <- function(path, msg){
  write(msg, path, append = TRUE)
}