# ==========================================================
# GET THE SAMPLE TRIPLETS
# ==========================================================

getCh1TraPreTripletsAndSynScores <- function(){
  trainPath <- paste(DREAMDIR,"../Challenge Data/Drug Synergy Data/ch1_train_combination_and_monoTherapy.csv",sep = "")
  ch1LeadPath <- paste(DREAMDIR,"sample_submission/ch1_leaderboard-prediction.csv",sep = "")
  trainData <- read.csv(trainPath,header = FALSE,stringsAsFactors=FALSE)
  ch1LeadData <- read.csv(ch1LeadPath,header = FALSE,stringsAsFactors=FALSE)
  
  trainTriplets <- as.matrix(trainData[2:nrow(trainData),1:3])
  trainTriplets[,c(1,2,3)] <- trainTriplets[,c(2,3,1)]
  # ch1LeadTriplets
  
  ch1Cell <- matrix(ch1LeadData[2:nrow(ch1LeadData),1],ncol = 1)
  ch1DrugPairList <- t(matrix(unlist(strsplit(ch1LeadData[2:nrow(ch1LeadData),2],"[.]")),nrow = 2))
  ch1LeadTriplets <- cbind(ch1DrugPairList,ch1Cell)
  
  ch1TraPreTriplets <- rbind(trainTriplets,ch1LeadTriplets)
  rowNames <- 1:nrow(ch1TraPreTriplets)
  ch1TraPreTriplets <- data.frame(ch1TraPreTriplets,row.names = rowNames)
  names(ch1TraPreTriplets) <- c("drugA","drugB","cell")
  
  # synergyScores <- as.numeric(trainData[2:nrow(trainData),12],ncol = 1)
  synergyScores <- as.numeric(trainData[2:nrow(trainData),12])
  synergyScores <- matrix(synergyScores,ncol = 1)
  
  ch1TraPreTripletsAndSynScores <- list()
  ch1TraPreTripletsAndSynScores$traPreTri <- ch1TraPreTriplets
  ch1TraPreTripletsAndSynScores$synSco <- synergyScores
  
  return(ch1TraPreTripletsAndSynScores)
}

DIR_PATH <- paste(DREAMDIR, "/sample_submission/", sep = "")
CH1_COMB_PRIO <- "ch1_final-combination_priority.csv"
CH1_PRED <- "ch1_final-prediction.csv"
CH1_CONFI <- "ch2_final-confidence_matrix.csv"
CH1_SYN <- "ch2_final-synergy_matrix.csv"

get_c1_comb_prio_pair <- function(){
  raw <- read.csv(paste(DIR_PATH, CH1_COMB_PRIO, sep=""),
                  head=TRUE,
                  stringsAsFactors=FALSE)
  pair_df <- data.frame(matrix(0, nrow = nrow(raw), ncol = 2))
  for(i in 1:nrow(raw)){
    pair <- strsplit(raw[i,1],"[.]")[[1]]
    pair_df[i,1] <- pair[1]
    pair_df[i,2] <- pair[2]
  }
  return(pair_df)
}

get_c1_pred_trip <- function(){
  raw <- read.csv(paste(DIR_PATH, CH1_PRED, sep=""),
                  head=TRUE,
                  stringsAsFactors=FALSE)
  trip_df <- data.frame(matrix(0, nrow = nrow(raw), ncol = 3))
  for(i in 1:nrow(raw)){
    pair <- strsplit(raw[i,2],"[.]")[[1]]
    trip_df[i,1] <- pair[1]
    trip_df[i,2] <- pair[2]
    trip_df[i,3] <- raw[i,1]
  }
  return(trip_df)
}

get_c2_trip <- function(){
  raw_syn <- raw <- read.csv(paste(DIR_PATH, CH1_SYN, sep=""),
                             head=FALSE,
                             stringsAsFactors=FALSE)
  raw_confi <- raw <- read.csv(paste(DIR_PATH, CH1_CONFI, sep=""),
                               head=FALSE,
                               stringsAsFactors=FALSE)
  
  raw_syn_drug <- raw_syn[2:nrow(raw_syn),1]
  raw_syn_cell <- raw_syn[1, 2:ncol(raw_syn)]
  #   raw_confi_drug <- raw_confi[2:nrow(raw_confi),1]
  #   raw_confi_cell <- raw_confi[1, 2:ncol(raw_confi)]
  
  trip_df <- data.frame(matrix(nrow = length(raw_syn_drug) * length(raw_syn_cell), ncol = 3))
  
  for(i in 1:length(raw_syn_drug)){
    for(j in 1:length(raw_syn_cell)){
      pair <- strsplit(raw_syn_drug[i],"[.]")[[1]]
      trip_df[(i-1)*length(raw_syn_cell)+j,1] <- pair[1]
      trip_df[(i-1)*length(raw_syn_cell)+j,2] <- pair[2]
      trip_df[(i-1)*length(raw_syn_cell)+j,3] <- raw_syn_cell[j]
    }
  }
  return(trip_df)
}

# ==========================================================
# QUERY FEATURES FROM DATABASE
# ==========================================================

# get cell features
getCFeatures <- function (cellDF, feature) {
  connection <- connectDreamDB()
  names(cellDF)[1] <- "cell"
  dbWriteTable(conn = connection, "tempHOHO", value = cellDF, append = FALSE, row.names = FALSE)
  query <- paste("select t.cell,
                  cf.feature,
                  c.value
                  from tempHOHO as t 
                  left join cell_name as cn on t.cell = cn.name
                  left join cell as c on c.cell_id = cn.id
                  left join cell_feature as cf on c.feature_id = cf.id
                  or c.value is null
                  where cf.feature ='", feature, "';", sep = "")
  results <- getQueryResults(connection, query)
  dbSendQuery(conn = connection, "drop table tempHOHO;")
  dbDisconnect(connection)
  return(results)
}

# get drug features
getDFeatures <- function (drugDF, feature) {
  connection <- connectDreamDB()
  names(drugDF)[1] <- "drug"
  dbWriteTable(conn = connection, "tempYOHO", value = drugDF, append = FALSE, row.names = FALSE)
  query <- paste("select t.drug,
                  df.feature,
                  d.value
                  from tempYOHO as t 
                  left join drug_name as dn on t.drug = dn.name
                  left join drug as d on d.drug_id = dn.id
                  left join drug_feature as df on d.feature_id = df.id
                  or d.value is null
                  where df.feature ='", feature, "';", sep = "")
  results <- getQueryResults(connection, query)
  dbSendQuery(conn = connection, "drop table tempYOHO;")
  dbDisconnect(connection)
  return(results)
}

# given data frame with drug1, drug2 pair,
# return a data frame of drug pair related features with the following column format
# drug1, drug2, feature1, feature2, .... featureN
getDDFeatures <- function (drugPairDF, feature) {
  connection <- connectDreamDB()
  names(drugPairDF)[1] <- "drug1"
  names(drugPairDF)[2] <- "drug2"
  dbWriteTable(conn = connection, "tempYOLO", value = drugPairDF, append = FALSE, row.names = FALSE)
  query <- paste("select t.drug1, 
                 t.drug2, 
                 ddf.feature, 
                 dd.value 
                 from tempYOLO as t 
                 left join drug_name as dn on t.drug1 = dn.name 
                 left join drug_name as dn2 on t.drug2 = dn2.name 
                 left join drug_drug as dd on dd.drugA_id = dn.id and dd.drugB_id = dn2.id 
                 left join drug_drug_feature as ddf on dd.feature_id = ddf.id
                 or dd.value is null
                 where ddf.feature = '", feature, "';", sep = "")
  results <- getQueryResults(connection, query)
  dbSendQuery(conn = connection, "drop table tempYOLO;")
  dbDisconnect(connection)
  return(results)
}

# given data frame with drug, cell pair,
# return a data frame of drug, cell pair related features with the following column format
# drug, cell, feature1, feature2, .... featureN
getDCFeatures <- function (drugPairDF, feature) {
  connection <- connectDreamDB()
  names(drugPairDF)[1] <- "drug"
  names(drugPairDF)[2] <- "cell"
  dbWriteTable(conn = connection, "tempYOYO", value = drugPairDF, append = FALSE, row.names = FALSE)
  query <- paste("select t.drug, 
                  t.cell,                   
                  dcf.feature,                   
                  dc.value                   
                  from tempYOYO as t 
                  left join drug_name as dn on t.drug = dn.name
                  left join cell_name as cn on t.cell = cn.name
                  left join drug_cell as dc on dc.drug_id = dn.id and dc.cell_id = cn.id
                  left join drug_cell_feature as dcf on dc.feature_id = dcf.id
                  or dc.value is null  
                 where dcf.feature = '", feature, "';", sep = "")
  results <- getQueryResults(connection, query)
  dbSendQuery(conn = connection, "drop table tempYOYO;")
  dbDisconnect(connection)
  return(results)
}

# ==========================================================
# PREPARE SAMPLES FOR CHALLENGE 1 AND 2
# ==========================================================

applyTF <- function(E) {
  drug1 <- E[1]
  drug2 <- E[2]
  print(paste(E[1], ".", E[2], sep = ""))
  return(getScores(drug1, drug2))
}

getUniqueScores <- function(E, uniqueScores) {
  COMBID <- paste(E[1], ".", E[2], sep="")
  return(uniqueScores[COMBID, ])
}

# training_triplets <- getCh1TraPreTripletsAndSynScores()
triplets1 <- get_c1_pred_trip()
triplets2 <- get_c2_trip()
triplets_train <- getCh1TraPreTripletsAndSynScores[[1]][1:length(getCh1TraPreTripletsAndSynScores[[2]]),]
prep <- function(triplets) {
  ddPairs <- triplets[1:2]
  d1cPairs <- cbind(triplets[1], triplets[3])
  d2cPairs <- cbind(triplets[2], triplets[3])
  
  # get cell featurest
  cfList <- unlist(SUB_GEX[1])
  c1Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(cfList)))
  for (i in 1:length(cfList)){
    c1Features[,i] <- getCFeatures(d1cPairs[2], cfList[i])[3]
  }
    
  # get drug features
  dfList <- c("Molecular.Weight",
              "XLogP3",
              "Hydrogen.Bond.Donor.Count", 
              "Hydrogen.Bond.Acceptor.Count",	
              "Rotatable.Bond.Count",
              "Exact.Mass",	
              "Topological.Polar.Surface.Area", 
              "Heavy.Atom.Count",
              "Complexity"
              )
  d1Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dfList)))
  d2Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dfList)))  
  for (i in 1:length(dfList)){
    d1Features[,i] <- getDFeatures(d1cPairs[1], dfList[i])[3]
    d2Features[,i] <- getDFeatures(d2cPairs[1], dfList[i])[3]
  }
    
  # loop through 85 cell lines
  # 85*3 cols per drug-cell pair
  cell_info <- read.csv(paste("../Challenge Data/Sanger Molecular Data/cell_info.csv"),header = TRUE,stringsAsFactors=FALSE)
  cell_names <- cell_info[1]
    
    
  # get drug1 cell features
  dcfList <- c("ic50_normalized_avg",
               "einf_normalized_avg",
               "h_normalized_avg")
  d1cFeatures <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dcfList)*85))
  d2cFeatures <- data.frame(matrix(nrow = nrow(d2cPairs), ncol = length(dcfList)*85))
  
  for (i in 1:nrow(cell_names)){
    cell_name <- cell_names[i,1]
    d1_all_cells <- d1cPairs
    d1_all_cells[,2] <- cell_name
    d2_all_cells <- d2cPairs
    d2_all_cells[,2] <- cell_name
    for (j in 1:length(dcfList)){
      d1cFeatures[,i*3-3+j] <- getDCFeatures(d1_all_cells, dcfList[j])[4]
      d2cFeatures[,i*3-3+j] <- getDCFeatures(d2_all_cells, dcfList[j])[4]
    }
  }  
  
  # get drug drug features
  ddfList <- c("target_gene_similarity",
               "target_pathway_similarity")
  ddFeatures <- data.frame(matrix(nrow = nrow(ddPairs), ncol = length(ddfList)))
  for (i in 1:length(ddfList)){
    ddFeatures[,i] <- getDDFeatures(ddPairs, ddfList[i])[4] # only want the value
  }
  
  # we have 17 columns for each of the drug drug pairs
  allTargetFeatures <- matrix(rep(NA, 17*dim(ddPairs)[1]), nrow = dim(ddPairs)[1], ncol = 17, byrow = TRUE)
  uniquePairs <- unique(ddPairs)
  uniqueScores <- t(apply(uniquePairs, 1, applyTF))
  uniqueID <- apply(uniquePairs, 1, paste, collapse = ".")
  rownames(uniqueScores) <- uniqueID
  allTargetFeatures <- t(apply(ddPairs, 1, getUniqueScores))
  
  ddFeatures <- cbind(ddFeatures, allTargetFeatures)
  
  # synergies <- getCh1TraPreTripletsAndSynScores()[[2]]
  partZ1 <- cbind(ddFeatures, c1Features)
  return(partZ1)
  
  # need to take care of symmetry
#   featurePart1 <- data.frame(d1Features, d1cFeatures, d2Features, d2cFeatures, ddFeatures, c1Features)
#   featurePart1Tra <- featurePart1[1:length(synergies),]
#   featurePart2 <- data.frame(d2Features, d2cFeatures, d1Features, d1cFeatures, ddFeatures, c1Features)
#   featurePart2Tra <- featurePart2[1:length(synergies),]
} 

triplets <- triplets1
triplets <- triplets2
triplets <- triplets_train

ddPairs <- triplets[1:2]
d1cPairs <- cbind(triplets[1], triplets[3])
d2cPairs <- cbind(triplets[2], triplets[3])

# get cell features
cfList <- unlist(SUB_GEX[1])
c1Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(cfList)))
for (i in 1:length(cfList)){
  c1Features[,i] <- getCFeatures(d1cPairs[2], cfList[i])[3]
}

# get drug features
dfList <- c("Molecular.Weight",
            "XLogP3",
            "Hydrogen.Bond.Donor.Count", 
            "Hydrogen.Bond.Acceptor.Count",	
            "Rotatable.Bond.Count",
            "Exact.Mass",	
            "Topological.Polar.Surface.Area", 
            "Heavy.Atom.Count",
            "Complexity"
)
d1Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dfList)))
d2Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dfList)))  
for (i in 1:length(dfList)){
  d1Features[,i] <- getDFeatures(d1cPairs[1], dfList[i])[3]
  d2Features[,i] <- getDFeatures(d2cPairs[1], dfList[i])[3]
}

# loop through 85 cell lines
# 85*3 cols per drug-cell pair
cell_info <- read.csv(paste("../Challenge Data/Sanger Molecular Data/cell_info.csv"),header = TRUE,stringsAsFactors=FALSE)
cell_names <- cell_info[1]

# get drug1 cell features
dcfList <- c("ic50_normalized_avg",
             "einf_normalized_avg",
             "h_normalized_avg")
d1cFeatures <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dcfList)*85))
d2cFeatures <- data.frame(matrix(nrow = nrow(d2cPairs), ncol = length(dcfList)*85))

getForAllCellLines <- function(E, dcPairs) {
  d_all_cells <- dcPairs
  d_all_cells[,2] <- E
#  d2_all_cells <- d2cPairs
#  d2_all_cells[,2] <- E
#  features <- data.frame(matrix(rep(NA, dim(d1cPairs)[1]*3), ncol = 3, nrow = dim(d1cPairs)[1]))
  features <- getDCFeatures(d_all_cells, dcfList[1])[4]
#  for (j in 1:length(dcfList)){
#    features[,j] <- getDCFeatures(d1_all_cells, dcfList[j])[4]
#    d2cFeatures[,i*3-3+j] <- getDCFeatures(d2_all_cells, dcfList[j])[4]
#  }  
  print("getting features....")
  return(features)
}

# apply for 85 cell lines
#temp <- data.frame(matrix(rep(unlist(cell_names), dim(d1cPairs)[1]), ncol = dim(cell_names)[1], nrow = dim(d1cPairs)[1], byrow = TRUE))
# temp <- data.frame(matrix(rep(unlist(cell_names[1:3,]), dim(d1cPairs)[1]), ncol = 3, nrow = dim(d1cPairs)[1], byrow = TRUE))
#test <- apply(temp, 2, getForAllCellLines, dcPairs = d1cPairs)

temp <- data.frame(matrix(rep(unlist(cell_names), dim(d1cPairs)[1]), ncol = dim(cell_names)[1], nrow = dim(d1cPairs)[1], byrow = TRUE))
for (j in 1:length(dcfList)){
  d1cFeatures[,(85*(j-1)+1):(85*j)] <- apply(temp, 2, getForAllCellLines, dcPairs = d1cPairs)
  d2cFeatures[,(85*(j-1)+1):(85*j)] <- apply(temp, 2, getForAllCellLines, dcPairs = d2cPairs)
}

d_dc <- data.frame(d1Features, d1cFeatures, d2Features, d2cFeatures)
write.table(d_dc, file = "./druggy/input/predictionInput-ch2-f-d_dc.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  

# for (i in 1:nrow(cell_names)){
#   cell_name <- cell_names[i,1]
#   d1_all_cells <- d1cPairs
#   d1_all_cells[,2] <- cell_name
#   d2_all_cells <- d2cPairs
#   d2_all_cells[,2] <- cell_name
#   for (j in 1:length(dcfList)){
#     d1cFeatures[,i*3-3+j] <- getDCFeatures(d1_all_cells, dcfList[j])[4]
#     d2cFeatures[,i*3-3+j] <- getDCFeatures(d2_all_cells, dcfList[j])[4]
#   }
# }  

# get drug drug features
ddfList <- c("target_gene_similarity",
             "target_pathway_similarity")
ddFeatures <- data.frame(matrix(nrow = nrow(ddPairs), ncol = length(ddfList)))
for (i in 1:length(ddfList)){
  ddFeatures[,i] <- getDDFeatures(ddPairs, ddfList[i])[4] # only want the value
}

# we have 17 columns for each of the drug drug pairs
allTargetFeatures3 <- matrix(rep(NA, 17*dim(ddPairs)[1]), nrow = dim(ddPairs)[1], ncol = 17, byrow = TRUE)
uniquePairs <- unique(ddPairs)
uniqueScores3 <- t(apply(uniquePairs, 1, applyTF))
uniqueID <- apply(uniquePairs, 1, paste, collapse = ".")
rownames(uniqueScores3) <- uniqueID
allTargetFeatures3 <- t(apply(ddPairs, 1, getUniqueScores, uniqueScores = uniqueScores3))

ddFeatures3 <- cbind(ddFeatures, allTargetFeatures3)

# synergies <- getCh1TraPreTripletsAndSynScores()[[2]]
partZ3 <- cbind(ddFeatures3, c1Features)

write.table(partZ1, file = "./druggy/input/predictionInput-ch1-f-dd_c.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  
write.table(partZ2, file = "./druggy/input/predictionInput-ch2-f-dd_c.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  
write.table(partZ3, file = "./druggy/input/trainingInput-f-dd_c.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  

# combine monotherapy features with the rest
# featuresetDir <- paste(DREAMDIR, "druggy/input/monotherapy_featureset", sep = "/")
# partY1 <- read.csv(file = paste(featuresetDir, "ch1-features.csv", sep = "/"))
# rowname_part1 <- apply(triplets_train, 1, paste, collapse = ".")
# rowname_part2 <- apply(cbind(triplets_train[,2], triplets_train[,1], triplets_train[,3]), 1, paste, collapse = ".")
# synergies <- getCh1TraPreTripletsAndSynScores()[[2]]
# half<-dim(d_dc)[2]/2
# training_features_part1 <- cbind(synergies, d_dc, partZ3)
# training_features_part2 <- cbind(synergies, d_dc[,(half+1):(half*2)],d_dc[,1:half], partZ3)
# names(training_features_part2)<-names(training_features_part1)
# #rownames(training_features_part1) <- 1:dim(triplets_train)[1]
# #rownames(training_features_part2) <- (dim(triplets_train)[1]+1):(2*dim(triplets_train)[1])
# rownames(training_features_part1) <- make.names(rowname_part1, unique = TRUE)
# rownames(training_features_part2) <- make.names(rowname_part2, unique = TRUE)
# training_features <- rbind(training_features_part1, training_features_part2)
load("/media/ehsueh/Data/projects/dream/src/Drug-Combination-Prediction-2015/druggy/input/big_features.rdata")
# write.table(training_features_part1, file = "./druggy/input/trainingInput-final1.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
# write.table(training_features_part2, file = "./druggy/input/trainingInput-final1.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
# training_feature <- read.delim(file = "./druggy/input/trainingInput-final1.csv", stringsAsFactors = FALSE, sep = ",")

# save(training_features_part1, training_features_part2, file = "./druggy/input/fina_training_features.RData")
# 
# triplets <- triplets2
# ddPairs <- triplets[1:2]
# d1cPairs <- cbind(triplets[1], triplets[3])
# d2cPairs <- cbind(triplets[2], triplets[3])
# 
# 

# combine drug, drug_cell
# taking care of symmetry of the train set
train_dc <- read.csv(file = "./druggy/input/monotherapy_featureset/train-features-dc.csv", stringsAsFactors = FALSE, sep = ",", header = FALSE)[,4:513]
ch1_dc <- read.csv(file = "./druggy/input/monotherapy_featureset/ch1-features-dc.csv", stringsAsFactors = FALSE, sep = ",", header = FALSE)[,4:513]
ch2_dc <- read.csv(file = "./druggy/input/monotherapy_featureset/ch2-features-dc.csv", stringsAsFactors = FALSE, sep = ",", header = FALSE)[,4:513]

half<-dim(train_dc)[2]/2
ch1_set <- cbind(ch1_dc[1:half], d1Features, ch1_dc[half:(half*2)], d2Features, partZ1)
save(ch1_set, file = "./druggy/input/ch1_set.RData")

ch2_set <- cbind(ch2_dc[1:half], d1Features, ch2_dc[half:(half*2)], d2Features, partZ2) # re-run to get d1Features and d2Features from corresponding triplets
save(ch2_set, file = "./druggy/input/ch2_set.RData")

# all features. No PCA.
train_set_part1 <- cbind(synergies, train_dc[1:half], d1Features, train_dc[half:(half*2)], d2Features, partZ3)
train_set_part2 <- cbind(synergies, train_dc[half:(half*2)], d2Features, train_dc[1:half], d1Features, partZ3)
names(train_set_part1) <- names(train_set_part2)
train_set <- rbind(train_set_part1, train_set_part2)
save(train_set, file = "./druggy/input/train_set.RData")

# write to files as csv
write.table(ch1_set, file = "./druggy/input/predictionInput-ch1-final_all.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  
write.table(ch2_set, file = "./druggy/input/predictionInput-ch2-final_all.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  
write.table(train_set, file = "./druggy/input/trainingInput-train-final_all.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  

# no dd target, no cell gex
train_set_part1 <- cbind(synergies, train_dc[1:half], d1Features, train_dc[half:(half*2)], d2Features)
train_set_part2 <- cbind(synergies, train_dc[half:(half*2)], d2Features, train_dc[1:half], d1Features)
names(train_set_part1) <- names(train_set_part2)
train_set <- rbind(train_set_part1, train_set_part2)
write.table(train_set, file = "./druggy/input/trainingInput-train-final_no_dd_c.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  
write.table(train_set, file = "./druggy/input/trainingInput-train-final_all_impute.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  

# pca 80, 85, 90
raw <- read.csv(file = "./druggy/input/master_feature_pca_80.csv", stringsAsFactors = FALSE, sep = ",", header = FALSE)
# raw <- read.csv(file = "./druggy/input/master_feature.csv", header = FALSE, stringsAsFactors = FALSE)
train_end <- length(synergies)
ch1_end <- train_end + dim(partZ1)[1] 
ch2_end <- ch1_end + dim(partZ2)[1]
train_dc <- raw[1:train_end,]
ch1_dc <- raw[(train_end + 1):ch1_end,]
ch2_dc <- raw[(ch1_end + 1):ch2_end,]
train_set_part1 <- cbind(synergies, train_dc, d1Features, d2Features, partZ3)
train_set_part2 <- cbind(synergies, train_dc, d2Features, d1Features, partZ3)
train_set <- rbind(train_set_part1, train_set_part2)
write.table(train_set, file = "./druggy/input/trainingInput-train-final_pca80.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")  
ch1_set <- cbind(ch1_dc, ch1_d1Features, ch1_d2Features, partZ1)
ch2_set <- cbind(ch2_dc, ch2_d1Features, ch2_d2Features, partZ2)
write.table(ch1_dc, file = "./druggy/input/predictionInput-ch1-final_pca80.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")
write.table(ch2_dc, file = "./druggy/input/predictionInput-ch2-final_pca80.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")

ch1_set <- cbind()

read.csv(file = "/media/ehsueh/Data/projects/dream/src/Drug-Combination-Prediction-2015/druggy/input/predictionInput-ch2-final_all_fixed.csv", stringsAsFactors = FALSE)

tr_gex_tar_only <- read.csv(file = "druggy/input/trainingInput-f-dd_c.csv", header = FALSE)
train_gex_tar <- cbind(synergies, tr_gex_tar_only)
write.table(train_gex_tar, file = "druggy/input/trainingIniput-final-noMono.csv", row.names = FALSE, col.names = FALSE, append = FALSE, sep = ",")

