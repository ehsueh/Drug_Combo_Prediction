# master.R should have set the working directory

write_to_master_log <- function(id, xval, date, log_file_path){
  new_row <- paste(id, xval, date, log_file_path, sep = ",0")
  write.csv(new_row, "log/master_log.csv", append = TRUE)
}

write_to_log_file <- function(path, msg){
  write.csv(msg, path, append = TRUE)
}