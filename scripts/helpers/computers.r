
weighted_distance <- function(diff_mat, e1, e2) {
  # Takes in a matrix of element-wise differences between two matrices
  # Also takes in two vectors of eigen values for the PCs of the two matrices
  # Returns a scalar representing the mean of the column-wise Euclidean distances
    # Weighted by the eigenvalues
  
  euclids <- sqrt(colSums(diff_mat^2))
  rel_var <- (e1 + e2) / sum(e1, e2)
  return(sum(euclids * rel_var))
}