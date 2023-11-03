rm(list=ls())
library(tidyverse)

csvs <- list.files('scripts/descriptives/fmri-eeg_data/csv')
csvs <- csvs[!csvs %in% 'concatenate.r']
d <- data.frame()

file_code <- read.csv('scripts/descriptives/fmri-eeg_data/file_code.csv')

repair_missing <- function(d, data) {
  # If d has more columns than data
  missing_cols <- colnames(d)[!colnames(d) %in% colnames(data)]
  for (col in missing_cols) {
    data[col] <- NA
  }
  return(data)
}

for (file in csvs) {
  params <- unlist(strsplit(file, '_'))
  subject <- file_code[file_code$filename==file,]$subject
  run <- file_code[file_code$filename==file,]$run
  data <- read.csv(paste0('scripts/descriptives/fmri-eeg_data/csv/', file))
  data['subject'] <- subject
  data['run'] <- run
  # Catch missing data
  d <- tryCatch({
    rbind(d, data)
  }, error = function(e) {
    data <- repair_missing(d, data)
    rbind(d, data)
  })
}

# Move subject and run to first columns
d <- cbind(d[,c('subject', 'run')], d[, 1:(ncol(d)-2)])
# Save data
write.csv(d, 'scripts/descriptives/fmri-eeg_data/pilot.csv', row.names=FALSE)

