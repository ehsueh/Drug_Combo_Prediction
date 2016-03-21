# master.R
# 
# Purpose: Scripts for running the pipeline from preparing feature sets to achiving the submission files
# Description:
#   - calls the execution functions from 5 components: feature prep, train, predict, format and archive
#   - logs important results after each component for future reference and reproducibility purposes
# ================================================================================


# ========================================
# CONFIGURATION
# ========================================
# set parameters for the pipeline

setwd("/home/zack/Drug_Combo_Prediction")
# set working directory to location of Drug_Combo_Prediction folder

XVAL2TRAIN_RATIO <- 0.3
# proportion of the training set used for cross validation
# range: 0 to 1

PCA_FEATURE_NAMES <- c()
# vector of features used to train model and predict result which require PCA
# range: 

NON_PCA_FEATURE_NAMES <- c()
# vector of features used to train model and predict result which does not require PCA
# range: 

MODEL_PARAM_START <- list()
# list of model parameters that map to their corresponding value or range
# the pipeline will find the optimal value for all parameter to train the model
# range: 
# Example: list(h=c(300,300,300), e=1000)

MODEL_PARAM_END <- list()
# list of model parameters that map to their corresponding value or range
# the pipeline will find the optimal value for all parameter to train the model
# range: 
# Example: list(h=c(300,300,300), e=1000)

COMPLETION_STAGE <- "S"
# desired terminating process in the pipeline
# range:
#   T - training process
#   P - predicting process
#   F - formatting process
#   S - submitting process


# ========================================
# LIBRARY INSTALLATION
# ========================================

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


# ========================================
# FILE SOURCING
# ========================================

source("/prep/feature_prep.R")
source("/train/h2o.R")
source("/predict/h2o.R")
source("/format/formatter.R")
source("/archive/zipper.R")


# ========================================
# INITIALIZE LOG
# ========================================
# require to have master_log.csv and logs/ in Drug_Combo_Prediction/log/ before running this section

# master_log_header <- paste("log id", "xval r2", "date", "log file path", "training time")
######### Assume this file with header already exist!!!!!!!!!!#########################


# ========================================
# FEATURE PREPARATION
# ========================================

load("/feature_bank/monotherapy_normalized_avg_imputed.RData")
# from feature_bank folder loads multiple feature matrices that are separated by functionality
# dimension:
#   row - 1794 + 1089 + 31535 (training feature set + ch 1a feature set + ch 2 feature set without duplication)
#   col - depends like the feature group

t_xval_ch1_ch2_feature_set <- prep_run(PCA_FEATURE_NAMES, NON_PCA_FEATURE_NAMES)
# prep_run returns a vector of four .csv file paths that store h2o formated matrices for training, xval, ch1 and ch2 feature sets

######## MODIFY SO THAT WE DON'T PREP ALL THE MATRICES CUZ THERE IS THE STAGE CONFIG!!!!!!!


# ========================================
# NEURAL NETWORK TRAINING
# ========================================

best_model <- train_run(t_xval_ch1_ch2_feature_set[1], t_xval_ch1_ch2_feature_set[2], MODEL_PARAM)
# train_run takes 3 parameters: paths to the training and xval set and the model parameter list
# train_run returns the dnn model that gives the best xval score


# ========================================
# PREDICTION
# ========================================

ch1_prediction_score <- predict_run(best_model, t_xval_ch1_ch2_feature_set[3])
ch2_prediction_score <- predict_run(best_model, t_xval_ch1_ch2_feature_set[4])


# ========================================
# FORMAT
# ========================================

ch1_formatted_prediction_score <- format_ch1(ch1_prediction_score, final = TRUE)
ch2_formatted_prediction_score <- format_ch2(ch2_prediction_score, final = TRUE)
# format_x convert the data into proper format and store it in .csv file
# format_x return the path to the csv file
# x determines the format: either challlenge 1 or challenge 2 format (ch1 or ch2)
# final (boolean) indicates whether to format for the final round or for leaderboard rounds

# ========================================
# ARCHIVE
# ========================================

archive_run(ch1_confidence_path, ch1_formatted_prediction_path, 1)
archive_run(ch2_confidence_path, ch2_formatted_prediction_path, 2)
# input: confidence path, prediction path, challenge index (1 is 1a and 2 is 2)
