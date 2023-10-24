
weighted_distance <- function(diff_mat, e1, e2) {
  # Takes in a matrix of element-wise differences between two matrices
  # Also takes in two vectors of eigen values for the PCs of the two matrices
  # Returns a scalar representing the mean of the column-wise Euclidean distances
    # Weighted by the eigenvalues
  
  euclids <- sqrt(colSums(diff_mat^2))
  rel_var <- (e1 + e2) / sum(e1, e2)
  return(sum(euclids * rel_var))
}

drop_suffix <- function(d) {
  # Drop everything after underscore from variable name
  out <- sapply(colnames(d), FUN=function(x) unlist(strsplit(x, '_'))[1])
  return(out)
}

custom_scale <- function(response, response_m, response_sd) {
  # Check to ensure there's a non zero sd
  scaled <- (response - response_m) / response_sd
  out <- ifelse(response_sd != 0 & !is.na(response_sd), scaled, response_m)
  return(out)
}

normalize_subject_item <- function(d) {
  # Normalize within subject and item
  gather_cols <- colnames(d)
  gather_cols <- gather_cols[!gather_cols %in% c('subject', 'item', 'conf')]
  d <- d %>% 
    mutate(id = 1:(nrow(d))) %>% 
    gather(item, response, gather_cols[1]:gather_cols[length(gather_cols)]) %>% 
    group_by(subject, item) %>% 
    mutate(response_m = mean(response), response_sd = sd(response)) %>% 
    ungroup() %>% 
    mutate(response_sc = custom_scale(response, response_m, response_sd)) %>% 
    select(-response, -response_m, -response_sd) %>% 
    rename(response = response_sc) %>% 
    spread(item, response) %>% 
    select(-id) 
  return(d)
}

