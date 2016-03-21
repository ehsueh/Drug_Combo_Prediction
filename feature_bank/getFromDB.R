# ==========================================================
# GET THE SAMPLE TRIPLETS
# ==========================================================
# loading ch1_triplets, ch2_triplets and training_triplets_and_syn_score
FEATURE_DIR <- "./feature_bank/"
load(paste(FEATURE_DIR, "triplets.RData"))
# getting database utility functions
source(paste(FEATURE_DIR, "dbUtils.R"))

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
# OTHER HELPER FUNCTIONS
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

# save features as RData
saveFeature2RData <- function(featureName) {
  ch1 <- get(paste("ch1_", featureName, sep = ""))
  ch2 <- get(paste("ch2_", featureName, sep = ""))
  tra <- get(paste("train_", featureName, sep = ""))
  assign(featureName, rbind(ch1, ch2, tra))
  save(list = featureName, file = paste(FEATURE_DIR, "/", featureName, ".RData", sep = ""))
}


# ==========================================================
# PREPARE SAMPLES FOR CHALLENGE 1 AND 2
# ==========================================================
num_samples <- dim(ch1_triplets)[1] + dim(ch2_triplets)[1] + dim(training_triplets_and_syn_scores)[1]

cfList <- unlist(SUB_GEX[1])

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
drug_pchem <- data.frame(matrix(nrow = num_samples, ncol = (length(dfList) * 2)))
train_triplets <- training_triplets_and_syn_scores[,1:3]

# Other features can be obtained from DB and saved as RData in 
# a similar method as show below
for (set in list("ch1", "ch2", "train")) {
  
  triplets <- get(paste(set, "triplets", sep = "_"))

  ddPairs <- triplets[1:2]
  d1cPairs <- cbind(triplets[1], triplets[3])
  d2cPairs <- cbind(triplets[2], triplets[3])
  
  # get drug features
  # save drug physical chemical properties 
  # into drug_pchem.RData
  d1Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dfList)))
  d2Features <- data.frame(matrix(nrow = nrow(d1cPairs), ncol = length(dfList)))  
  for (i in 1:length(dfList)){
    d1Features[,i] <- getDFeatures(d1cPairs[1], dfList[i])[3]
    d2Features[,i] <- getDFeatures(d2cPairs[1], dfList[i])[3]
  }
  
  var <- paste(set, "drug_pchem", sep = "_")
  assign(var, cbind(d1Features, d2Features))
  
}

saveFeature2RData("drug_pchem")
