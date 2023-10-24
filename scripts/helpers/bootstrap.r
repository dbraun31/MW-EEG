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

get_rmat_score <- function(matches, matches_gt, data) {
  # Takes in matches and matches_gt
  # Determines loadings with get_matches()
  # Compare matches against ground truth matches weighted by eigenvalues
  
  score <- 0
  eigens <- eigen(cov(data))$values
  
  idx <- 0
  for (PC in names(matches_gt)){
    idx <- idx + 1
    prop_correct <- sum(matches[[PC]] %in% matches_gt[[PC]]) / length(matches_gt[[PC]])
    score <- score + prop_correct * (eigens[idx]/sum(eigens))
    
  }
  return(score)
}

get_composite_result <- function(mask_size, subject, nrows, subject_result) {
  # Takes in subject number, number of rows and subject result object
  # with attrs: rmat_score (vector), true_eigens (vector), and PC1:6
  # Returns one row of data frame with subject, nrows, mean_rmat_score, mean_eigen_score
  
  mean_rmat_score <- mean(subject_result[['rmat_score']])
  mean_eigen_score <- 0
  true_eigens <- subject_result[['true_eigens']]
  
  # Compute eigen score
  # (ie, was the ground truth eigenvalue within the 95%CI bootstrap interval)
  idx <- 0
  for (PC in names(subject_result[['eigen_dist']])) {
    idx <- idx + 1
    CIs <- quantile(subject_result[['eigen_dist']][[PC]], probs = c(.025, .975))
    true_eigen <- true_eigens[idx]
    recovered <- ifelse(true_eigen > CIs[1] & true_eigen < CIs[2], 1, 0)
    # Weight recovery by the proportion explained variance
    mean_eigen_score <- mean_eigen_score + recovered * (true_eigen / sum(true_eigens))
  }
  return(data.frame(mask_size = mask_size,
                    subject=subject, 
                    nrows=nrows,
                    mean_rmat_score=mean_rmat_score, 
                    mean_eigen_score=mean_eigen_score))
}

bootstrap_simulation <- function(ds, result_list) {
  # Conduct bootstrap resampling and evaluation for one sample 
  
  data <- ds[sample(nrow(ds), replace=TRUE),]
  rmat <- prcomp(data)$rotation
  eigens <- eigen(cov(data))$values
  matches <- get_matches(rmat)
  # Evaluate bootstrap sample against ground truth
  rmat_score <- get_rmat_score(matches, matches_gt, data)
  result_list[['rmat_score']][sim] <- rmat_score
  result_list[['true_eigens']] <- eigens_gt
  idx <- 0
  # Populate the appropriate PC's simulation with that PC's eigenvalue
  for (col in colnames(rmat)) {
    idx <- idx+1
    result_list[['eigen_dist']][[col]][sim] <- eigens[idx]
  }
  
  return(result_list)
  
}