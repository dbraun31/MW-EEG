rm(list=ls())
library(tidyverse)
source('scripts/helpers/computers.r')

d <- read.csv('data/behavioral/MW_EEG_behavioral_full.csv')

# Keep only response vars
d <- d[, c('subject', 'run', 
         grep('*_response$', colnames(d), value=TRUE))]

colnames(d)[3:(ncol(d))] <- drop_suffix(d[,3:(ncol(d))])

# Reverse code
# Recoding image, ling, conf
# See https://github.com/dbraun31/MW-EEG/blob/master/docs/mw_eeg_data_dictionary.md

d$image <- 100 - d$image
d$ling <- 100 - d$ling
d$conf <- 100 - d$conf


# Save
write.csv(d, 'data/behavioral/MW_EEG_behavioral.csv', row.names=FALSE)
