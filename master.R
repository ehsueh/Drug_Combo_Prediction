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

# set working directory to location of Drug_Combo_Prediction folder
MASTERDIR <- "/media/ehsueh/Data/projects/dream/refactored-codes/Drug_Combo_Prediction/"
# MASTERDIR <- "/home/zack/Drug_Combo_Prediction/"
setwd(MASTERDIR)

RUN_NAME <- "test"
LOG_PATH <- paste("./log/logs/", RUN_NAME, ".txt", sep = "")

TRAIN2XVAL_RATIO <- 0.75
# proportion of the training set used for cross validation
# range: 0 to 1

TOTAL_FEATURES <- c("monotherapy_normalized_avg_imputed", "drug_pchem", "string_gex")

PCA_FEATURES <- c("monotherapy_normalized_avg_imputed")
# vector of features used to train model and predict result which require PCA
# range: 

SWAP_FEATURES <- c("monotherapy_normalized_avg_imputed", "drug_pchem")

MODEL_PARAM_START <- list(hidden=c(300, 300, 300), epoch=1000)
# list of model parameters that map to their corresponding value or range
# the pipeline will find the optimal value for all parameter to train the model
# range: 
# Example: list(h=c(300,300,300), e=1000)

MODEL_PARAM_END <- list(hidden=c(300, 300, 300), epoch=1000)
# list of model parameters that map to their corresponding value or range
# the pipeline will find the optimal value for all parameter to train the model
# range: 
# Example: list(h=c(300,300,300), e=1000)

COMPLETION_STAGE <- "A"
# desired terminating process in the pipeline
# range:
#   T - training process
#   P - predicting process
#   F - formatting process
#   A - archiving process


# ========================================
# LIBRARY INSTALLATION
# ========================================

if (! require(h2o, quietly=TRUE)) {
  install.packages("h2o",
                   repos="http://cran.us.r-project.org")
  library(h2o)
}

library(stringr)


# ========================================
# FILE SOURCING
# ========================================

source("./prep/prep.R")
source("./train/train.R")
source("./predict/predict.R")
source("./format/formatter.R")
source("./archive/zipper.R")
source("./log/logger.R")

# ========================================
# INITIALIZE LOG FILE & MASTER LOG ENGRY
# ========================================
# require to have master_log.csv and logs/ in Drug_Combo_Prediction/log/ before running this section

write_to_log_file("Run Name")
write_to_log_file(paste("\t", RUN_NAME,sep = ""))
write_to_log_file("Training to Cross Validation Ratio")
write_to_log_file(paste("\t", TRAIN2XVAL_RATIO,sep = ""))
write_to_log_file("Total Features")
write_to_log_file(paste("\t", TOTAL_FEATURES,sep = ""))
write_to_log_file("PCA Features")
write_to_log_file(paste("\t", PCA_FEATURES,sep = ""))
write_to_log_file("Swap Features")
write_to_log_file(paste("\t", SWAP_FEATURES,sep = ""))
write_to_log_file("Model Parameter Start")
write_to_log_file(paste("\t", MODEL_PARAM_START,sep = ""))
write_to_log_file("Model Parameter End")
write_to_log_file(paste("\t", MODEL_PARAM_END,sep = ""))
write_to_log_file("Completion Stage")
write_to_log_file(paste("\t", COMPLETION_STAGE,sep = ""))

master_log_entry_head <- c(LOG_PATH, list(run_name=RUN_NAME, 
                                          train2xval_ratio=TRAIN2XVAL_RATIO, 
                                          total_features=TOTAL_FEATURES,
                                          pca_features=PCA_FEATURES,
                                          swap_features=SWAP_FEATURES,
                                          model_param_start=MODEL_PARAM_START,
                                          model_param_end=MODEL_PARAM_END,
                                          completion_stage=COMPLETION_STAGE))


# ========================================
# FEATURE PREPARATION
# ========================================

# from feature_bank folder loads multiple feature matrices that are separated by functionality
# dimension:
#   row - 1794 + 1089 + 31535 (training feature set + ch 1a feature set + ch 2 feature set without duplication)
#   col - depends like the feature group


t_xval_ch1_ch2_feature_set <- prep_run(TOTAL_FEATURES, PCA_FEATURES, SWAP_FEATURES, TRAIN2XVAL_RATIO, use_pred_for_pca = TRUE, base_name = RUN_NAME)
# input 
#   TOTAL_FEATURES:   vector containing all the features for training
#   PCA_FEATURES:     vector containing features to pca
#   SWAP_FEATURES:    vector containing features to swap for symmetry
#   TRAIN2XVAL_RATIO: proportion of new training set to original training set
#   use_pred_for_pca: boolean value indicating whether prediction feature sets are involved in pca
#   base_name:        base file name for prepared training, xval, ch1 and ch2 feature set
# return
#   vector of strings containing paths to the four h2o formatted .csv files, training, xval, ch1 and ch2
# side effect
#   1. save the four h2o formatted .csv files inside prep folder


# ========================================
# NEURAL NETWORK TRAINING
# ========================================

best_model <- train_run(t_xval_ch1_ch2_feature_set[1], t_xval_ch1_ch2_feature_set[2], MODEL_PARAM_START, MODEL_PARAM_END, master_log_entry_head, TRUE)
# input
#   t_xval_ch1_ch2_feature_set[1]:  feature set for training models
#   t_xval_ch1_ch2_feature_set[2]:  feature set for cross validating the trained model
#   MODEL_PARAM_START:              start value for model training parameter range, same as end if it's a value
#   MODEL_PARAM_END:                end value for model training parameter range, same as start if it's a value
#   TRUE:                           boolean value indicating whether the training result should be saved in database
# return
#   trained h2o model 
# side effect
#   1. save the model in model folder of train folder
#   2. log the result in master_log.csv and in a separate .txt formated log file


# ========================================
# PREDICTION
# ========================================

if(COMPLETION_STAGE!=T){
  ch1_prediction_score <- predict_run(best_model, t_xval_ch1_ch2_feature_set[3])
  ch2_prediction_score <- predict_run(best_model, t_xval_ch1_ch2_feature_set[4])
}
# input
#   best_model:                     the trained h2o model for prediction
#   t_xval_ch1_ch2_feature_set[3]:  feature set for prediction
# return
#   prediction score
# side effect
#   1. save prediction in pred folder


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

if(COMPLETION_STAGE=='A'){
  archive_run(ch1_confidence_path, ch1_formatted_prediction_path, 1)
  archive_run(ch2_confidence_path, ch2_formatted_prediction_path, 2)
}
# input
#   ch1_confidence_path:            path to the confidence score .csv file
#   ch1_formatted_prediction_path:  path to the properly formatted prediction .csv file
#   1:                              integer value of either 1 or 2 indicating the challenge index for naming purposes
# side effect
#   1. save the zip file ready for submission in archive folder
