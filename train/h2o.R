# ==========================================================
# CONFIGURE INPUTS AND OUTPUTS
# ==========================================================
# setwd(DREAMDIR) # if haven't already
# INPUT_TRAIN <- paste(DREAMDIR, "./druggy/input/trainingInput-train-final_all.csv", sep ="/")
INPUT_TRAIN <- paste(DREAMDIR, "./druggy/input/trainingInput-train-final_all_impute.csv", sep ="/")
# only target net and cell gex
# INPUT_TRAIN <- "/media/ehsueh/Data/projects/dream/src/Drug-Combination-Prediction-2015/druggy/input/trainingIniput-final-noMono.csv"

# no drug drug target topology features and no cell gex features
# INPUT_TRAIN<- "./druggy/input/trainingInput-train-final_no_dd_c.csv"

# pca 80
INPUT_TRAIN <- "./druggy/input/trainingInput-train-final_pca80.csv"

# INPUT_TRAIN <- "/home/ehsueh/Desktop/zack/master_feature.csv"
INPUT_PRED <- paste(DREAMDIR, "./druggy/input/predictionInput-ch1-final_pca80.csv", sep ="/")
# INPUT_PRED_CH2 <- paste(DREAMDIR, "./druggy/input/predictionInput-ch2-final_all_fixed.csv", sep ="/")
INPUT_PRED_CH2 <- paste(DREAMDIR, "./druggy/input/predictionInput-ch2-final_pca80.csv", sep ="/")
OUTPUT_DIR <- paste(DREAMDIR, "./druggy/output", sep ="/")

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
# TRAINING AND PREDICTION
# ==========================================================
# set up a local cluster with 1GB RAM

localH2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
#s<- h2o.importFile(INPUT_TRAIN)
dataH2o <- h2o.importFile(INPUT_TRAIN)
#idx <- sample(1:nrow(dataH2o), round(nrow(dataH2o)*3/4))
dataTrainH2o <- dataH2o[idx,]
dataXValH2o <- dataH2o[-idx,1:ncol(dataH2o)]
targetXValH2o <- dataH2o[-idx,1]

dataPreH2o <- h2o.importFile(INPUT_PRED)

dataPreH2o2 <- h2o.importFile(INPUT_PRED_CH2)

# Rectifier works the best

# training with target network and cell gex features only
# hidden: 50, epoch: 500
# MSE:  193.285
# R2 :  0.7357674
# Mean Residual Deviance :  193.285
nn_dd_c1 <- h2o.deeplearning(x = 2: ncol(dataH2o),
                             y = 1,
                             training_frame = dataTrainH2o,
                             validation_frame = dataXValH2o,
                             activation = "RectifierWithDropout",
                             input_dropout_ratio = 0.2,
                             hidden_dropout_ratios = c(0.5),
                            # adaptive_rate = ,
                            # rate = 0.01,
                            # stopping_rounds = 6,
                            # stopping_metric = "AUTO",
                            # distribution = "gaussian",
                             hidden = c(50), 
                             epochs = 1000) 

# all with out pca
# hidden: 50, epochs: 500
# MSE:  546.9972
# R2 :  0.3223694
# Mean Residual Deviance :  546.9972
# hidden: 50, epochs: 100
# MSE:  457.5325
# R2 :  0.4332001
# Mean Residual Deviance :  457.5325

# pca 85
# hidden: 50, epoch: 1000
# MSE:  359.6036
# R2 :  0.5312114
# Mean Residual Deviance :  359.6036
# hidden: 50, epoch: 500
# MSE:  390.1242
# R2 :  0.4914241
# Mean Residual Deviance :  390.1242
# hidden: 500, epoch: 1000
# MSE:  186.3771
# R2 :  0.757034
# Mean Residual Deviance :  186.3771
nn_run_history <- data.frame(row.names = c("H", "E", "train_mse", "train_r2", "xval_mse", "xval_r2"), stringsAsFactors = FALSE)
header <- c("H", "E", "train_mse", "train_r2", "xval_mse", "xval_r2")

# H2ORegressionMetrics: deeplearning
# ** Reported on training data. **
#   Description: Metrics reported on full training frame
# 
# MSE:  46.75496
# R2 :  0.9407132
# Mean Residual Deviance :  46.75496
# 
# 
# H2ORegressionMetrics: deeplearning
# ** Reported on validation data. **
#   Description: Metrics reported on full validation frame
# 
# MSE:  284.4604
# R2 :  0.5775356
# Mean Residual Deviance :  284.4604

h <- c(300,300,300)
e <- 1000
nn <- h2o.deeplearning(x = 2: ncol(dataH2o),
                       y = 1,
                       training_frame = dataTrainH2o,
                       validation_frame = dataXValH2o,
                       activation = "RectifierWithDropout",
                       input_dropout_ratio = 0.2,
                       hidden_dropout_ratios = c(0.3, 0.3, 0.3),
                       hidden = c(h), 
                       epochs = e)

h <- 60
e <- 1000
nn <- h2o.deeplearning(x = 2: ncol(dataH2o),
                       y = 1,
                       training_frame = dataTrainH2o,
                       validation_frame = dataXValH2o,
                       activation = "RectifierWithDropout",
                       input_dropout_ratio = 0.2,
                       hidden_dropout_ratios = c(0.3, 0.3, 0.3),
                       # adaptive_rate = TRUE,
                       # not as good
                       # adaptive_rate = FALSE,
                       # momentum_start = 0.5,
                       # momentum_stable = 0.99,
                       # rate = 0.01,
                       # no good
                       # stopping_rounds = 6,
                       # stopping_metric = "AUTO",
                       # distribution = "gaussian",
                       hidden = c(h), 
                       epochs = e)


new <- cbind(h, e, h2o.mse(nn,train = TRUE), h2o.r2(nn,train = TRUE), h2o.mse(nn,valid = TRUE), h2o.r2(nn,valid = TRUE))
colnames(new) <- header
nn_run_history <- rbind(nn_run_history, new)

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

# h_list <- c(50, 275, 500, 750, 1000)
# e_list <- c(250, 500, 1000, 1500, 2000)
h_list <- c(50, 500, 1000)
e_list <- c(500, 1000, 1500)
best_nn <- nn
best_r2 <- 0
mse_dd_c <- matrix(NA, nrow = length(h_list), ncol = length(e_list))
r2_dd_c <- matrix(NA, nrow = length(h_list), ncol = length(e_list))
for (i in 1:length(h_list)) {
  for (j in 1:length(e_list)){
    h <- h_list[i]
    e <- e_list[j]
    print(paste(h, e, sep = ","))
    nn <- train(h,e)
    r2_dd_c[i,j] <- h2o.r2(nn)
    mse_dd_c[i,j] <- h2o.mse(nn)
    if (h2o.r2(nn) > best_r2) {
      best_r2 <- h2o.r2(nn)
      best_nn <- nn
    }
    print(paste(h2o.r2(nn), h2o.mse(nn), sep = ","))
  }
}

nn_best <- nn
#epoch = 4000, we get 0.7057 on cross val (0.8817 according to H2o)
#epoch = 1000, 0.7124, 0.8755
#epoch = 500, rate = 0.01, 0.6901, 0.8570

h2oPredictions <- h2o.predict(nn, dataPreH2o)

tree <- h2o.randomForest(x = 2:ncol(dataH2o),
                         y = 1,
                         training_frame = dataTrainH2o,
                         validation_frame = dataXValH2o,
                         sample_rate = 0.2,
                         max_depth = 100)

h2oPredictionsTree <- h2o.predict(tree, dataPreH2o)

# view performance stats
h2o.performance(nn1)
h2o.performance(tree)

h2oXValPredictions <- h2o.predict(nn1, dataXValH2o)
y <- as.numeric(unlist(as.data.frame(h2oXValPredictions))) >= 20
y_ <- as.numeric(unlist(as.data.frame(targetXValH2o))) >= 20
xval <- length(which((y == y_) == TRUE))/length(y)
# can also compare sensitivity and specificity

# ==========================================================
# VISUALIZATION
# ==========================================================

# convert all prediction elements to a single vector
nnPredictions <- as.numeric(unlist(as.data.frame(h2oPredictions)))
rfPredictions <- as.numeric(unlist(as.data.frame(h2oPredictionsTree)))

nnWeight <- 0.5
rfWeight <- 0.5

# ensemble, takes weighted average of the two methods
avPredictions <- (nnWeight * nnPredictions) + (rfWeight * rfPredictions)

nnDF <- data.frame(prediction = nnPredictions)
rfDF <- data.frame(prediction = rfPredictions)
avDF <- data.frame(prediction = avPredictions)

nnDF$method <-"NN" 
rfDF$method <-"RF"
avDF$method <-"AV"

# plot graph
allPred <- rbind(nnDF, rfDF, avDF)
ggplot(allPred, aes(prediction, fill = method)) + geom_histogram(alpha = 0.3, position = 'identity')

# ==========================================================
# WRITE OUTPUT TO FILE
# ==========================================================

formatRound1 <- function(predictions) {
  names(predictions) <- "PREDICTION"
  # raw <- read.csv(paste("./sample_submission/ch1_leaderboard-prediction.csv"),header = TRUE,stringsAsFactors=FALSE)  
  raw <- read.csv(paste("./sample_submission/ch1_final-prediction.csv"),header = TRUE,stringsAsFactors=FALSE)
  df <- data.frame(raw[1], raw[2], predictions)
  names(df) <- names(raw)
  return(df)
}

formatRound2 <- function(predictions) {
  # raw <- read.csv(paste("./sample_submission/ch2_leaderboard-synergy_matrix.csv"),header = TRUE,stringsAsFactors=FALSE)
  raw <- read.csv(paste("./sample_submission/ch2_final-synergy_matrix.csv"),header = TRUE,stringsAsFactors=FALSE)
  bin <- ifelse(matrix(data = predictions, ncol = 85, nrow = length(predictions)/85, byrow = TRUE) > 20, 1, 0)
  df <- rbind(str_replace_all(names(raw), pattern = "\\.", "-")[-1], bin)
  df <- cbind(rbind("", raw[1]), df)
  return(df)
}

formatRound2Binary <- function(predictions) {
  return(formatRound2(predictions)>30)
}

# write output to file
write.csv(formatRound1(rfPredictions), file = paste(OUTPUT_DIR, "rfPredictionOutput.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE)
write.table(formatRound1(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-final-ch1-nn3.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
write.csv(formatRound1(avPredictions), file = paste(OUTPUT_DIR, "avPredictionOutput.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE)
write.csv(formatRound2(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-ch2-score-r3.2.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE)
write.table(formatRound2(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-ch2-binary20-r4.4.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")

write.table(formatRound1(nnPredictions), file = paste(OUTPUT_DIR, "nnPredictionOutput-final-ch1-nn_pca80.csv", sep = "/"), row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")
write.table(formatRound2(nnPredictions2), file = paste(OUTPUT_DIR, "nnPredictionOutput-final-ch2-nn_pca80.csv", sep = "/"), row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
