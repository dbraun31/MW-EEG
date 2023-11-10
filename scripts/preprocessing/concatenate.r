rm(list=ls())
library(tidyverse)

csvs <- list.files('data/behavioral_data/csv')
d <- data.frame()

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
  subject <- params[3]
  run <- params[4]
  data <- read.csv(paste0('data/behavioral_data/csv/', file))
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
write.csv(d, 'data/behavioral_data/MW_EEG_behavioral_full.csv', row.names=FALSE)

