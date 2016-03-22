# if data has N/A, impute it first before running PCA
# e.g. if raw_features contains N/A... 
# imputed_feature <- as.data.frame(apply(raw_feature, 2, replace_non_num))
replace_non_num <- function(E){
  result <- E
  selection_list <- result[result!="N/A"]
  if(length(selection_list)==0){
    selection_list <- c(0)
  }
  non_num_index <- which(E=="N/A")
  for(index in non_num_index){
    result[index] <- sample(selection_list,1,replace=TRUE)
  }
  return(as.numeric(result))
}

sym_expand <- function(uniq_train_set, swap = FALSE) {
  part1 <- uniq_train_set
  part2 <- part1
  if (swap == TRUE) { # swap drug1 features and drug2 features
    half <- dim(uniq_train_set)[2]/2 # must have even number of columns 
    part2 <- cbind(uniq_train_set[,(half+1):(half*2)], uniq_train_set[,1:half])  
  }
  return(rbind(part1,part2))
}

# swap_list are the features (drug related) that needs to be duplicated in a special way (swap) to account for symmetry
prep_run <- function(union_set, pca_list, swap_list, train2xval_ratio, use_pred_for_pca, base_name) {
  write_to_log_file("######################## PREPARATION STARTS #####################")

  FEATURE_DIR <- paste(MASTERDIR, "./feature_bank/", sep = "")
  OUT_DIR <- paste(MASTERDIR, "./prep/h2o_feature/", sep = "")
  
  # load relevant triplets (training, ch1 and ch2 for final round)
  load(paste(FEATURE_DIR, "triplets.RData", sep = ""))
  # number of samples for the different sets
  NUM_UNIQ_TRAIN <- dim(training_triplets_and_syn_scores)[1] # unique training set
  NUM_PRED_CH1 <- dim(ch1_triplets)[1] # challenge 1 prediction set
  NUM_PRED_CH2 <- dim(ch2_triplets)[1] # challenge 2 prediction set
  
  train_file <- paste(OUT_DIR, base_name, "_train.csv", sep = "")
  xval_file <- paste(OUT_DIR, base_name, "_xval.csv", sep = "")
  pred1_file <- paste(OUT_DIR, base_name, "_pred1.csv", sep = "")
  pred2_file <- paste(OUT_DIR, base_name, "_pred2.csv", sep = "")  

  # logging
  write_to_log_file("Output:")
  write_to_log_file(train_file)
  write_to_log_file(xval_file)
  write_to_log_file(pred1_file)
  write_to_log_file(pred2_file)
  
  # split training set into training and cross validation sets based on indicated ratio
  train_idx <- sample(1:NUM_UNIQ_TRAIN, round(NUM_UNIQ_TRAIN*train2xval_ratio))
  train_synergies <- training_triplets_and_syn_scores[train_idx,4]
  train_synergies <- c(train_synergies, train_synergies)
  xval_synergies <- training_triplets_and_syn_scores[-train_idx,4]
  
  train_all <- NULL
  xval_all <- NULL
  pred1_all <- NULL
  pred2_all <- NULL
  
  for (path in union_set) {
    load(paste(FEATURE_DIR, path, ".RData", sep = ""))
    set <- get(path)
#     if ("N/A"%in%set) { # impute N/A  note: can expand this to look for other strings or look for NULL or NA
#       set <- as.data.frame(apply(set, 2, replace_non_num))
#     }
    
    temp <- set[1:NUM_UNIQ_TRAIN,]
    train <- temp[train_idx,]
    xval <- temp[-train_idx,]
    pred1 <- set[(NUM_UNIQ_TRAIN+1):(NUM_UNIQ_TRAIN + NUM_PRED_CH1),]
    pred2 <- set[(NUM_UNIQ_TRAIN + NUM_PRED_CH1+1):(NUM_UNIQ_TRAIN + NUM_PRED_CH1 + NUM_PRED_CH2),]    
    
    # prepare training set
    # train set must take into account of symmetry
    train <- sym_expand(train, path%in%swap_list)
    
    # if set needs pca 
    if (path%in%pca_list) {
      data_for_pca <- train # pca with train data and prediction data (no xval data)
      if (use_pred_for_pca) {
        data_for_pca <- rbind(data_for_pca, pred1, pred2)
      } 
      pca <- prcomp(formula = ~., data = data_for_pca)
      # keeping enough principle components to cover 80% proportion of variance
      # 80% was experimentally found to yield the best result during the manual runs
      num_cols <- which(cumsum(pca$sdev^2 / sum(pca$sdev^2)) > 0.80) [1]
      train <- predict(pca, train)[,1:num_cols]
      xval <- predict(pca, xval)[,1:num_cols]
      pred1 <-predict(pca, pred1)[,1:num_cols]
      pred2 <-predict(pca, pred2)[,1:num_cols]  
    }
    
    train_all <- cbind(train_all, train)
    xval_all <- cbind(xval_all, xval)
    pred1_all <- cbind(pred1_all, pred1)
    pred2_all <- cbind(pred2_all, pred2)
    
  }
  
  # attach synergies scores as label for train and xval set
  train <- cbind(train_synergies, train)
  xval <- cbind(xval_synergies, xval)
  
  write.table(train, train_file , col.names = FALSE, row.names = FALSE, sep = ",")
  write.table(xval, xval_file, col.names = FALSE, row.names = FALSE, sep = ",")
  write.table(pred1, pred1_file, col.names = FALSE, row.names = FALSE, sep = ",")
  write.table(pred2, pred2_file, col.names = FALSE, row.names = FALSE, sep = ",")
  write_to_log_file("######################## PREPARATION ENDS #######################")
  return(c(train_file, xval_file, pred1_file, pred2_file))
}