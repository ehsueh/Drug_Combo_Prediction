# Utility functions for access and updating the database
# Requires db and user be set up first
# Database name: dream2015

# Recommend downloading MySQL workbench https://dev.mysql.com/downloads/workbench/
#   to visualize and manage database
# If you didn't source your RStudio, use the next two lines to source it  
DREAMDIR <- "/media/ehsueh/Data/projects/dream/src/Drug-Combination-Prediction-2015/" 
setwd(DREAMDIR)

DB <- "dream2015"
USER <- "ehsueh"
PW <- "dream2015"
HOST <- "localhost"
DATADIR <- paste("../Challenge Data/", sep = "")

# ==========================================================
# RESOLVE DEPENDENCIES
# ==========================================================
# might need to execute this in the terminal first
# sudo apt-get install r-cran-rmysql
if (! require(DBI, quietly = TRUE)) {
  install.packages("DBI",
                   repos="http://cran.us.r-project.org")
  library(DBI)
}
if (! require(RMySQL, quietly=TRUE)) {
  install.packages("RMySQL",
                   repos="http://cran.us.r-project.org")
  library(RMySQL)
}
# ===========================================================
# COMMON FUNCTIONS
# ==========================================================
# opening db connection
connectDreamDB <- function () {
  connection <- dbConnect(MySQL(),
                          user = USER,
                          password = PW,
                          dbname = DB,
                          host = HOST)
  return (connection)
}

# helper function for getting query results
getQueryResults <- function(connection, query) {
  resultSet <- dbSendQuery(connection, query)
  results <- fetch(res = resultSet, n = -1) # -1 means get all rows
  if (dbHasCompleted(resultSet)) { # must close the result set before continuing
    dbClearResult(resultSet)
  }
  return (results)
}

# type can be "drug, cell, drug_drug, drug_cell, or drug_drug_cell"
addFeature <- function(feature, type) {
  if (type %in% c("drug", "cell", "drug_drug", "drug_cell", "drug_drug_cell")) {
    # insert if unique
    # else return the id of specified feature
    # TO-DO's: Not a priority, but it will be nice if the id does not increment when insertions failed.
    #          Right now, inserting duplicate values results in id incrementation. 
    #          I.e. we are left with ugly id's that jumps around.
    query <- paste("insert into ", type, "_feature(feature) values('", feature,
                   "') on duplicate key update id = last_insert_id(id), feature ='", feature, "';", sep = "")
    query1 <- "select last_insert_id();"
    dbSendQuery(connection, query)
    resultSet <- dbSendQuery(connection, query1)
    id <- fetch(resultSet, n = 1)
    dbClearResult(resultSet)
    return(id)
  } else {
    # maybe we should crash here
    print("Incorrect feature type.")
  }
}

# load the feature file given file path
#   type of feature 
#   and separater of the file
# takes in a file of the format:
#   DRUG/CELL (DRUG) (CELL) F1 F2 F3 .... FN
loadFeatureFile <- function(file, type, sep){ 
  raw <- read.csv(file = file,
                  head = TRUE,
                  stringsAsFactors = FALSE,
                  sep = sep) # this file is separated by backquotes
  nRows <- dim(raw)[1]
  nCols <- dim(raw)[2]
  # depending on the type of feature, the first n (up to 3 columns)
  #   are drug or cell names of which the feature belongs to.
  #   we will call these the header columns
  # the rest are feature columns
  headers <- strsplit(type, "_")[[1]]
  n <- length(strsplit(type,"_")[[1]])
  
  headerID <- matrix(nrow = nRows, ncol = n)
  
  # open db connection
  connection <- connectDreamDB()
  
  # get id's of first n cols
  for (j in 1:n) { # do it for each of the n header columns
    for (i in 1:nRows) { # do it for each row because some drug or cell might be new
      query <- paste("select id from ", headers[j], "_name where name = '", str_replace(raw[4,1], pattern = "\\.", replacement = "-"), "';", sep = "")
      headerID[i,j] <- getQueryResults(connection, query)[[1]]
      # TO-DO: if no such drug or cell.... add it and return the id
      # but we won't come across this case for the contest
      # so omit it for now
    }
    dfH <- data.frame(headerID)
    names(dfH)[j] <- paste(headers[j], "_id", sep = "")
  }
  
  # note, incoompatible data type will result in zeros
  # since features only take numeric values
  # NA --> 0.00000
  for (i in n:nCols){ # for each columns
    featureName <- names(raw)[i] 
    id <- addFeature(featureName, type)[[1]]
    feature_id <- rep(id, nRows)
    value <- raw[,i]
    df <- data.frame(dfH, feature_id, value)
    dbWriteTable(connection, value = df, name = type, append = TRUE, row.names = FALSE)
  }
  
}
