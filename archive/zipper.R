archive_run <- function(confidence_path, prediction_path, ch_index){
  zip(paste("DREAMToronto_", ch_index, sep = ""), c(confidence_path, prediction_path))
}