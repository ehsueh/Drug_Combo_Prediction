# ==========================================================
# INSTALL PACKAGE
# ==========================================================
if (! require(h2o, quietly=TRUE)) {
  install.packages("h2o",
                   repos="http://cran.us.r-project.org")
  library(h2o)
}
# if you get a RCurl missing dependency problem, install it:
# sudo apt-get install libcurl4-openssl-dev
if (! require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2",
                   repos="http://cran.us.r-project.org")
  library(ggplot2)
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
train_run <- function(train_path, xval_path, MODEL_PARAM_START, MODEL_PARAM_END) {
  # set up a local cluster with 1GB RAM
  local_h2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
  train_set <- importFile(train_path)
  xval_set <- importFile(xval_path)
  
  # after some runs with different param settings, 
  # these two are the only interesting 
  # params that end up improving the model so far
  h_start <- MODEL_PARAM_START$hidden
  e_start <- MODEL_PARAM_START$epochs
  h_end <- MODEL_PARAM_END$hidden
  e_end <- MODEL_PARAM_END$epochs
  h_inc <- (h_end - h_start)/5
  e_inc <- (e_end - e_start)/5
  
  best_nn <- NA # best neural net by validation R2
  best_r2 <- NA # R2 of the best neural net
  
  for (i in 1:5) {
    for (j in 1:5) {
      h <- h_start + h_inc*(i-1)
      e <- e_start + e_inc*(i-1)
      print(paste("Hidden layers: ", h, ", epochs: ", e, sep = ""))
      nn <- train(h,e)
      r2 <- h2o.r2(nn, valid = TRUE)
      if (r2 > best_r2) {
        best_r2 <- r2(nn)
        best_nn <- nn
      }
      # TODO: log this run
      print(paste("R2: ", r2, ", mse: ", mse, sep = ","))
    }
  }

}

# train <- function(h, e) {
#   nn <- h2o.deeplearning(x = 2: ncol(dataH2o),
#                               y = 1,
#                               training_frame = dataTrainH2o,
#                               validation_frame = dataXValH2o,
#                               activation = "RectifierWithDropout",
#                               input_dropout_ratio = 0.2,
#                               hidden_dropout_ratios = c(0.5),
#                               # adaptive_rate = TRUE,
#                             # not as good
#                               # adaptive_rate = FALSE,
#                               # momentum_start = 0.5,
#                               # momentum_stable = 0.99,
#                               # rate = 0.01,
#                             # no good
#                               # stopping_rounds = 6,
#                               # stopping_metric = "AUTO",
#                               # distribution = "gaussian",
#                               hidden = c(h), 
#                               epochs = e)
#   header <- col.names(nn_run_history)
#   new <- cbind(h, e, h2o.mse(nn,train = TRUE), h2o.r2(nn,train = TRUE), h2o.mse(nn,valid = TRUE), h2o.r2(nn,valid = TRUE))
#   nn_run_history <- rbind(nn_run_history, new)
#   colnames(nn_run_history) <- header
#   return(nn)
# }

# nn_run_history <- data.frame(row.names = c("hidden", "epochs", "train_mse", "train_r2", "xval_mse", "xval_r2"), stringsAsFactors = FALSE)
# header <- c("hidden", "epochs", "train_mse", "train_r2", "xval_mse", "xval_r2")


h2o.performance(nn1)
h2o.performance(tree)

h2oXValPredictions <- h2o.predict(nn1, dataXValH2o)
y <- as.numeric(unlist(as.data.frame(h2oXValPredictions))) >= 20
y_ <- as.numeric(unlist(as.data.frame(targetXValH2o))) >= 20
xval <- length(which((y == y_) == TRUE))/length(y)
# can also compare sensitivity and specificity
