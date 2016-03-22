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
# TRAINING FUNCTION
# ==========================================================
# returns the best model
# train_path (string) is the path of the training set .csv
# xval_path (string) is the path of the xval set .csv
# MODEL_PARAM is a list of params
#     e.g. MODEL_PARAM<-list(hidden:c(300,300,300), epochs:1000)

# MODEL_PARAM<-list(hidden:c(300,300,300), epochs:1000)
train_run <- function(train_path, xval_path, MODEL_PARAM_START, MODEL_PARAM_END, log_info, write_to_db = TRUE) {
  write_to_log_file("######################## TRAINING STARTS ########################")
  output_file <- paste(MASTERDIR, "./train/model/", RUN_NAME, "-model.RData", sep = "")
  write_to_log_file(paste("Output file: ", output_file))
# TO-DO: Not yet implemetned DB logging
#   if (write_to_db) {
#    source("./feature_bank/dbUtils.R")
#     setwd(MASTERDIR)
#   }
  
  # set up a local cluster with 1GB RAM
  # local_h2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
  train_set <- h2o.importFile(train_path)
  xval_set <- h2o.importFile(xval_path)
  
  # after some runs with different param settings, 
  # these two are the only interesting 
  # params that end up improving the model so far
  num_runs <- 3
  d <- MODEL_PARAM_START$dropout
  h_start <- MODEL_PARAM_START$hidden
  e_start <- MODEL_PARAM_START$epoch
  h_end <- MODEL_PARAM_END$hidden
  e_end <- MODEL_PARAM_END$epoch
  h_inc <- round((h_end - h_start)/num_runs)
  e_inc <- round((e_end - e_start)/num_runs)
  
  best_nn <- NULL # best neural net by validation R2
  best_r2 <- NULL # R2 of the best neural net
  
  for (i in 1:num_runs) {
    for (j in 1:num_runs) {
      h <- h_start + h_inc*(i-1)
      e <- e_start + e_inc*(i-1)
      print(paste("Hidden layers: ", h, ", epochs: ", e, sep = ""))
      nn <- h2o.deeplearning(x = 2: ncol(train_set),
                             y = 1,
                             training_frame = train_set,
                             validation_frame = xval_set,
                             activation = "RectifierWithDropout",
                             input_dropout_ratio = 0.2,
                             hidden_dropout_ratios = c(d),
                             hidden = c(h), 
                             epochs = e)
      
      r2 <- h2o.r2(nn, valid = TRUE)
      if (r2 > best_r2) {
        best_r2 <- r2(nn)
        best_nn <- nn
      }
      
      # log this run
      print(paste("XVal R2: ", r2, sep = ""))
      write_to_log_file(capture.output(summary(nn)))
      # log_info contains run name and feature selection
      hyperparam_d <- paste(d, collapse = ",")
      hyperparam_h <- paste(h, collapse = ",")
      hyperparam_e <- e
      write_to_master_log(log_info[1], paste(log_info[2], collapse = ","), hyperparam_d, hyperparam_h, hyperparam_e, h2o.r2(nn, train = TRUE), h2o.mse(nn, train = TRUE), h2o.r2(nn, valid = TRUE), h2o.mse(nn, valid = TRUE))      
      
      if (write_to_db) {
        # connect to db 
        con <- connectDreamDB()
        # write params and results to database
        # To-Do: finish stuff here
        # close connection
        dbDisconnect(con)
      }
    }
  }

  # saving results and logging
  save(best_nn, file = output_file)
  write_to_log_file("######################## TRAINING ENDS ##########################")

}
