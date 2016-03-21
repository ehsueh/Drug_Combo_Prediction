archive_run <- function(confidence_path, prediction_path, ch_index){
  folder_directory <- paste("ch", ch_index, sep = "")
  zip(paste("folder_directory/DREAMToronto_", ch_index,".zip", sep = ""), c(confidence_path, prediction_path))
}