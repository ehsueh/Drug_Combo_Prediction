
# ==========================================================
# WRITE OUTPUT TO FILE
# ==========================================================

format_ch1 <- function(predictions) {
  names(predictions) <- "PREDICTION"
  # raw <- read.csv(paste("./sample_submission/ch1_leaderboard-prediction.csv"),header = TRUE,stringsAsFactors=FALSE)  
  raw <- read.csv(paste("./sample_submission/ch1_final-prediction.csv"),header = TRUE,stringsAsFactors=FALSE)
  df <- data.frame(raw[1], raw[2], predictions)
  names(df) <- names(raw)
  return(df)
}

format_ch2 <- function(predictions) {
  # raw <- read.csv(paste("./sample_submission/ch2_leaderboard-synergy_matrix.csv"),header = TRUE,stringsAsFactors=FALSE)
  raw <- read.csv(paste("./sample_submission/ch2_final-synergy_matrix.csv"),header = TRUE,stringsAsFactors=FALSE)
  bin <- ifelse(matrix(data = predictions, ncol = 85, nrow = length(predictions)/85, byrow = TRUE) > 20, 1, 0)
  df <- rbind(str_replace_all(names(raw), pattern = "\\.", "-")[-1], bin)
  df <- cbind(rbind("", raw[1]), df)
  return(df)
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
