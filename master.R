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

FEATURE_NAMES <- c()
# vector of features used to train model and predict result
# range: 

PCA_

MODEL_PARAM <- list()
# list of model parameters that map to their corresponding value or range
# the pipeline will find the optimal value for all parameter to train the model
# range: 

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
# FEATURE PREPARATION
# ========================================

load("/feature_bank/monotherapy_normalized_avg_imputed.RData")
# from feature_bank folder loads multiple feature matrices that are separated by functionality
# dimension:
#   row - 1794 + 1089 + 31535 (training feature set + ch 1a feature set + ch 2 feature set without duplication)
#   col - depends like the feature group


