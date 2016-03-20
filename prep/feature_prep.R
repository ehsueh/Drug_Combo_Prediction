PARENT_FOLDER <- paste(DREAMDIR, "druggy/input/monotherapy_featureset/pca", sep = "/")
TRAIN <- "train-features-dc.csv"
CH1_FEATURE <- "ch1-features-dc.csv"
CH2_FEATURE <- "ch2-features-dc.csv"

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

train <- read.csv(paste(PARENT_FOLDER, TRAIN, sep = "/"), head=FALSE, stringsAsFactors=FALSE)
# take care of symmetry
# first 3 cols are drug, drug and cell names
half_idx <- (length(train) - 3)/2 + 3 # first half belong to monotherapy of drug1, second half is of drug 2
train_drug1_names <- train[,1]
train_drug2_names <- train[,2]
train_cell_names <- train[,3]
train_drug1_features <- train[,4:half_idx]
train_drug2_features <- train[,(half_idx + 1):length(train)]
# part one will be the original train, part two will be drug 1 and 2 swapped
train_part1 <- cbind(train_drug2_names, train_drug1_names, train_cell_names, train_drug2_features, train_drug1_features)
names(train_part1) <- names(train)
mono_train_sym <- rbind(train, train_part1)

ch1_feature <- read.csv(paste(PARENT_FOLDER, CH1_FEATURE, sep = "/"), head=FALSE, stringsAsFactors=FALSE)
ch2_feature <- read.csv(paste(PARENT_FOLDER, CH2_FEATURE, sep = "/"), head=FALSE, stringsAsFactors=FALSE)

temp_feature <- rbind(mono_train_sym[,4:length(mono_train_sym)], ch1_feature[,4:length(ch1_feature)], ch2_feature[,4:length(ch2_feature)])

master_feature <- as.data.frame(apply(temp_feature, 2, replace_non_num))
master_feature_pca <- prcomp(formula = ~., data = master_feature)
project_feature <- master_feature_pca$x

# paste(PARENT_FOLDER, CH2_FEATURE, sep = "")
save(master_feature_pca, file = paste(PARENT_FOLDER, "master_feature_pca.RData", sep = ""))

# pca_feature_matrix <- master_feature_pca$rotation
pca_feature_matrix <- project_feature

# Write features that account for 80%, 85% and 90% of the variance
# write.table(pca_feature_matrix[,1:82], paste(PARENT_FOLDER, "master_feature_pca_80.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
write.table(pca_feature_matrix[,1:82], paste("./druggy/input/master_feature_pca_80.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
write.table(pca_feature_matrix[,1:101], paste(PARENT_FOLDER, "master_feature_pca_85.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")
write.table(pca_feature_matrix[,1:127], paste(PARENT_FOLDER, "master_feature_pca_90.csv", sep = ""), col.names = FALSE, row.names = FALSE, sep = ",")

write.table(master_feature, file = "druggy/input/master_feature.csv", sep = ",", col.names = FALSE, row.names = FALSE)

# Making sure there is no leakage of information and that our cross validation step is actually valid....
# idx randomly generated in h2o.R
idx <- sample(1:nrow(mono_train_sym), round(nrow(mono_train_sym)*3/4))
project_feature <- predict(master_feature_pca, master_feature)

mono_train_set <- mono_train_sym[idx,4:length(train)]
mono_xval_set <- mono_train_sym[-idx,4:length(train)]
mono_train_pred <-rbind(mono_train_set, ch1_feature[,4:length(ch1_feature)], ch2_feature[,4:length(ch2_feature)])
# contains no mono features from cross validation set nor prediction sets (ch1 and ch2)
mono_pca_train_only <- prcomp(formula = ~., data = mono_train_set)
# contains no mono features from cross validation set but include monotherapy features from ch1 prediction set and ch2 prediction set
mon_pca_train_pred_only <- prcomp(formula = ~., data = mono_train_pred)

# To-do's
# need to expand training set to take care of symmetry before PCA
# read docs from boris and provide numbers
# try running cross validation again on the new pca features
# add in pathway features from boris
# code clean up
