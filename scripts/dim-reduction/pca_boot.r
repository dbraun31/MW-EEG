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

get_rmat_score <- function(matches, matches_gt, data) {
  # Need a function that takes in matches and matches_gt
  # and returns a score according to the rules above
  # weight by relative proportion of eigenvalues
  
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
  
  idx <- 0
  for (PC in names(subject_result[['eigen_dist']])) {
    idx <- idx + 1
    CIs <- quantile(subject_result[['eigen_dist']][[PC]], probs = c(.025, .975))
    true_eigen <- true_eigens[idx]
    recovered <- ifelse(true_eigen > CIs[1] & true_eigen < CIs[2], 1, 0)
    mean_eigen_score <- mean_eigen_score + recovered * (true_eigen / sum(true_eigens))
  }
  return(data.frame(mask_size = mask_size,
                    subject=subject, 
                    nrows=nrows,
                    mean_rmat_score=mean_rmat_score, 
                    mean_eigen_score=mean_eigen_score))
}




# Start with content-related items
number_spelling <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", 'Ten', 'Eleven', 'Twelve')
initial_mask <- colnames(d)
initial_mask <- initial_mask[!initial_mask %in% c('subject', 'conf')]

Nsims <- 1000
result <- data.frame()

for (mask_size in seq(2, length(initial_mask), by = 2)) {
  mask <- sample(initial_mask, mask_size)
  print(paste0('Mask size:', mask_size))

  for (subject in unique(d$subject)) {
    print(paste0('Subject ', subject))
    ds <- d[d$subject==subject, mask]
    rmat_gt <- prcomp(ds)$rotation
    eigens_gt <- eigen(cov(ds))$values
    matches_gt <- get_matches(rmat_gt)
    
    
    # On each iteration: 
      # store one composite score for rotation matrix
      # store eigenvalues for all PCs
    subject_result <- list()
    
    for (sim in 1:Nsims) {
      # Resample
      data <- ds[sample(nrow(ds), replace=TRUE),]
      rmat <- prcomp(data)$rotation
      eigens <- eigen(cov(data))$values
      matches <- get_matches(rmat)
      rmat_score <- get_rmat_score(matches, matches_gt, data)
      subject_result[['rmat_score']][sim] <- rmat_score
      subject_result[['true_eigens']] <- eigens_gt
      idx <- 0
      for (col in colnames(rmat)) {
        idx <- idx+1
        subject_result[['eigen_dist']][[col]][sim] <- eigens[idx]
      }
    }
    result <- rbind(result, get_composite_result(mask_size, subject, nrow(ds), subject_result))
    
  }

}

# Visualize result
min_mask <- min(result$mask_size)
max_mask <- max(result$mask_size)

result %>% 
  gather(metric, value, mean_rmat_score, mean_eigen_score) %>% 
  mutate(mask_size = paste(number_spelling[mask_size], 'Items')) %>% 
  mutate(metric = recode(metric, `mean_eigen_score` = 'Mean Eigenvalue Score',
                         `mean_rmat_score` = 'Mean PC Recovery Score')) %>% 
  mutate(mask_size = factor(mask_size, levels = paste(number_spelling[min_mask:max_mask], 'Items'))) %>% 
  ggplot(aes(x = nrows, y = value)) + 
  geom_jitter(width = .1) + 
  geom_smooth(method='lm', color='steelblue', fill = 'steelblue', alpha=.4) +
  labs(
    x = 'Number of Rows',
    y = 'Score',
    caption = paste0("Each point reflects ", Nsims, " samples of one subject's data.")
  ) +
  facet_grid(mask_size~metric) + 
  ylim(0, 1) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        strip.background = element_rect(fill=NA))

ggsave('figures/PCA_bootstrap.png', height=1000, width=1000, units='px', dpi=150)

m1 <- lm(mean_rmat_score ~ nrows * mask_size, data = result)
summary(m1)

# Number of rows seems to matter, mask size matters less