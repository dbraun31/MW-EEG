rm(list=ls())
library(tidyverse)
source('scripts/helpers/normalize.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')

### BOOTSTRAP FACTOR MATCHING RULES ###
# abs(loading) > .5 = match for first-occurring factor
# factor accuracy = number of bootstrap items matching original items / the number of original items for the factor
# weighted average for overall accuracy
# grab eigenvalues for all PCs

# do i bother trying to integrate loadings and eigens into one metric?
# do i vary over mask size?

# Normalize data
d <- normalize_subject_item(d)

get_matches <- function(rmat) {
  # Takes in a rotation matrix
  # Returns a list of matches (abs > .5) for all PCs
  
  result <- list()
  matched_items <- c()
  
  # Iterate over columns in the rotation matrix
  for (col in colnames(rmat)) {
    # Pull out data from a column
    data <- rmat[,col]
    # Get the names of items with absolute score > .5
    candidates <- names(data[abs(data) > .5])
    # Ensure those items aren't already matched
    matches <- candidates[!candidates %in% matched_items]
    # Ensure elements aren't empty
    if (length(matches) == 0) {
      matches <- ''
    }
    # Update previously matched items and results
    matched_items <- c(matched_items, matches)
    result[[col]] <- matches
  }
  
  return(result)
  
}

get_score <- function(matches, matches_gt) {
  # Need a function that takes in matches and matches_gt
  # and returns a score according to the rules above
  
  
}





# Start with content-related items
mask <- c('past', 'fut', 'self', 'ppl', 'aff', 'att')

Nsims <- 100

for (subject in unique(d$subject)) {
  ds <- d[d$subject==subject, mask]
  rmat_gt <- prcomp(ds)$rotation
  eigens_gt <- eigen(cov(ds))$values
  matches_gt <- get_matches(rmat_gt)
  
  # On each iteration: 
    # store one composite score for rotation matrix
    # store eigenvalues for all PCs
  result <- list(rmat_score = c(), eigen_dist = list())
  
  for (sim in 1:Nsims) {
    # Resample
    data <- sample(ds, size = nrow(ds), replace=TRUE)
    rmat <- prcomp(data)$rotation
    eigens <- eigen(cov(data))$values
    matches < get_matches(rmat)
    rmat_score <- get_score(matches, matches_gt)
    
    result[['rmat_score']][sim] <- rmat_score
    idx <- 0
    for (col in colnames(rmat)) {
      idx <- idx+1
      result[['eigen_dis']][[col]][sim] <- eigens[idx]
    }
  }
  
}