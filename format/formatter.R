
# ==========================================================
# WRITE OUTPUT TO FILE
# ==========================================================

format_ch1 <- function(predictions, final = FALSE) {
  write_to_log_file("######################## FORMATTING STARTS ######################")
  predictions <- unlist(predictions[,1])
  names(predictions) <- "PREDICTION"
  format_file <- paste(MASTERDIR,"./sample_submission/ch1_leaderboard-prediction.csv", sep = "")
  if (final == TRUE) { # format for final round, else, format for leaderboard
    format_file <- paste(MASTERDIR,"./sample_submission/ch1_final-prediction.csv", sep = "")
  }
  raw <- read.csv(format_file,header = TRUE,stringsAsFactors=FALSE)  
  df <- data.frame(raw[1], raw[2], predictions)
  names(df) <- names(raw)
  # saving results and logging
  output_file <- paste(MASTERDIR, "./format/formatted/", RUN_NAME, "-ch1_formatted.csv", sep = "")
  write.table(df, file = output_file, row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")
  write_to_log_file(paste("Output file: ", output_file))
  write_to_log_file("######################## FORMATTING ENDS ########################")
  return(output_file)
}

format_ch2 <- function(predictions, final = FALSE) {
  write_to_log_file("######################## FORMATTING STARTS ######################")
  predictions <- unlist(predictions[,1])
  format_file <- paste(MASTERDIR,"./sample_submission/ch2_leaderboard-synergy_matrix.csv", sep = "")
  if (final == TRUE) {
    format_file <- paste(MASTERDIR,"./sample_submission/ch2_final-synergy_matrix.csv", sep = "")
  }
  raw <- read.csv(format_file,header = TRUE,stringsAsFactors=FALSE)  
  bin <- ifelse(matrix(data = predictions, ncol = 85, nrow = length(predictions)/85, byrow = TRUE) > 20, 1, 0)
  df <- rbind(str_replace_all(names(raw), pattern = "\\.", "-")[-1], bin)
  df <- cbind(rbind("", raw[1]), df)
  # saving results and logging
  output_file <- paste(MASTERDIR, "./format/formatted/", RUN_NAME, "-ch2_formatted.csv", sep = "")
  write.table(df, file = output_file, row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
  write_to_log_file(paste("Output file: ", output_file))
  write_to_log_file("######################## FORMATTING ENDS ########################")
  return(output_file)
}

# write output to file
# write.csv(formatRound1(rfPredictions), file = paste(OUTPUT_DIR, "rfPredictionOutput.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE)
# write.table(formatRound1(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-final-ch1-nn3.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
# write.csv(formatRound1(avPredictions), file = paste(OUTPUT_DIR, "avPredictionOutput.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE)
# write.csv(formatRound2(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-ch2-score-r3.2.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE)
# write.table(formatRound2(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-ch2-binary20-r4.4.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
# 
# write.table(formatRound1(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-final-ch1-nn_pca80.csv", sep = "/"), row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")
# write.table(formatRound2(nnPredictions2), file = paste(OUTPUT_DIR, "nnPredictionOutput-final-ch2-nn_pca80.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
