FEATURE_DIR <- "./feature_bank/"
OUT_DIR <- "./prep/h2o_feature/"

# load relevant triplets (training, ch1 and ch2 for final round)
load(paste(FEATURE_DIR, "triplets.RData", sep = "/"))
# number of samples for the different sets
NUM_UNIQ_TRAIN <- dim(training_triplets_and_syn_scores)[1] # unique training set
NUM_PRED_CH1 <- dim(ch1_triplets)[1] # challenge 1 prediction set
NUM_PRED_CH2 <- dim(ch2_triplets)[1] # challenge 2 prediction set

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
prep_run <- function(train2xval_ratio, union_set, pca_list, swap_list, use_pred_for_pca, base_name) {
  
  # split training set into training and cross validation sets based on indicated ratio
  train_idx <- sample(1:NUM_UNIQ_TRAIN, round(NUM_UNIQ_TRAIN*train2xval_ratio))
  train_synergies <- training_triplets_and_syn_scores[train_idx,4]
  train_synergies <- rbind(train_synergies, train_synergies)
  xval_synergies <- training_triplets_and_syn_scores[-train_idx,4]
  
  for (path in union_set) {
    load(paste(FEATURE_DIR, path, ".RData", sep = ""))
    set <- get(path)
    if ("N/A"%in%set) { # impute N/A  note: can expand this to look for other strings or look for NULL or NA
      set <- as.data.frame(apply(set, 2, replace_non_num))
    }
    
    train <- set[train_idx,]
    xval <- set[-train_idx,]
    pred1 <- set[(NUM_UNIQ_TRAIN+1):(NUM_UNIQ_TRAIN + NUM_PRED_CH1),]
    pred2 <- set[(NUM_PRED_CH1+1):(NUM_UNIQ_TRAIN + NUM_PRED_CH1 + NUM_PRED_CH2),]    
    
    # prepare training set
    # train set must take into account of symmetry
    train <- sym_expand(train, swap = set%in%swap_list)
    
    # if set needs pca 
    if (set%in%pca_list) {
      data_for_pca <- train
      if (use_pred_for_pca) {
        data_for_pca <- rbind(data_for_pca, pred1, pred2)
      } 
      # pca with train data and prediction data (no xval data)
      # keeping enough principle components to cover 80% proportion of variance
      # 80% was experimentally found to yield the best result during the manual runs
      num_cols <- which(cumsum(pca$sdev^2 / sum(pca$sdev^2)) > 0.80) [1]
      pca <- prcomp(formula = ~., data = data_for_pca)
      train <- predict(pca, train)[,1:num_cols]
      xval <- predict(pca, xval)[,1:num_cols]
      pred1 <-predict(pca, pred1)[,1:num_cols]
      pred2 <-predict(pca, pred2)[,1:num_cols]  
    }
    
    # attach synergies scores as label for train and xval set
    train <- cbind(train_synergies, train)
    xval <- cbind(xval_synergies, xval)
    
    write.table(train, paste(OUT_DIR, base_name, "_train.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
    write.table(xval, paste(OUT_DIR, base_name, "_xval.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
    write.table(pred1, paste(OUT_DIR, base_name, "_pred1.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
    write.table(pred2, paste(OUT_DIR, base_name, "_pred2.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
    
  }
}