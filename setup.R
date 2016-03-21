PARENT_FOLDER <- "/home/zack/Druggy/"
dir.create(PARENT_FOLDER)

# ========================================
# CREATE ARCHIVE FOLDER
# ========================================
ARCHIVE_FOLDER <- paste(PARENT_FOLDER, "archive/", sep = "")
dir.create(ARCHIVE_FOLDER)
dir.create(paste(ARCHIVE_FOLDER, "ch1", sep = ""))
dir.create(paste(ARCHIVE_FOLDER, "ch2", sep = ""))

# ========================================
# CREATE FEATURE BANK FOLDER
# ========================================
FEATURE_BANK_FOLDER <- paste(PARENT_FOLDER, "feature_bank/", sep = "")
dir.create(FEATURE_BANK_FOLDER)

# ========================================
# CREATE FORMAT FOLDER
# ========================================
FORMAT_FOLDER <- paste(PARENT_FOLDER, "format/", sep = "")
dir.create(FORMAT_FOLDER)

# ========================================
# CREATE LOG FOLDER
# ========================================
LOG_FOLDER <- paste(PARENT_FOLDER, "log/", sep = "")
dir.create(LOG_FOLDER)
dir.create(paste(LOG_FOLDER, "logs", sep = ""))
write("log_id,train_r2,xval_r2,date,log_path",paste(LOG_FOLDER, "master_log.csv", sep = ""))

# ========================================
# CREATE PREDICT FOLDER
# ========================================
PREDICT_FOLDER <- paste(PARENT_FOLDER, "predict/", sep = "")
dir.create(PREDICT_FOLDER)

# ========================================
# CREATE PREP FOLDER
# ========================================
PREP_FOLDER <- paste(PARENT_FOLDER, "prep/", sep = "")
dir.create(PREP_FOLDER)

# ========================================
# CREATE TRAIN FOLDER
# ========================================
TRAIN_FOLDER <- paste(PARENT_FOLDER, "train/", sep = "")
dir.create(TRAIN_FOLDER)