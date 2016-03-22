# ==========================================================
# INSTALL PACKAGE
# ==========================================================
if (! require(h2o, quietly=TRUE)) {
  install.packages("h2o",
                   repos="http://cran.us.r-project.org")
  library(h2o)
}

library(stringr)

# ==========================================================
# PREDICTION
# ==========================================================
# returns prediction matrix where the first column is the predicted label
#     and the remaining are the prediction features
# model is the model to be used for prediction and
# prediction_path (string) is the path of the file storing the prediction samples
predict_run <- function(model, prediction_path) {
  write_to_log_file("######################## PREDICTION STARTS ######################")
  # set up a local cluster with 1GB RAM
  # localH2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
  prediction_set <- h2o.importFile(prediction_path)
  predictions <- h2o.predict(model, prediction_set)
  synergies <- as.numeric(unlist(as.data.frame(predictions)))
  results <- cbind(synergies, as.data.frame(prediction_set))
  # saving results and logging
  output_file <- paste("./predict/predictions/", RUN_NAME, "-predictions.RData", sep = "" )
  save(results, file = output_file)
  write_to_log_file(paste("Output file: ", output_file))
  write_to_log_file("######################## PREDICTION ENDS ########################")
  return(results)
}
