rm(list=ls())
library(tidyverse)
source('scripts/helpers/computers.r')
source('scripts/helpers/bootstrap.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')

### BOOTSTRAP FACTOR MATCHING RULES ###
# abs(loading) > .5 = match for first-occurring factor
# factor accuracy = number of bootstrap items matching original items / the number of original items for the factor
# weighted average for overall accuracy
# grab eigenvalues for all PCs

# Normalize data
d <- normalize_subject_item(d)

# mask = number of items
initial_mask <- colnames(d)
initial_mask <- initial_mask[!initial_mask %in% c('subject', 'conf')]

### SUBJECT-WISE BOOTSTRAP PCA ###
Nsims <- 1000
result <- data.frame()

# Iterate over item set sizes
for (mask_size in seq(2, length(initial_mask), by = 2)) {
  # Pull random items
  mask <- sample(initial_mask, mask_size)
  print(paste0('Mask size:', mask_size))

  # Iterate over subjects
  for (subject in unique(d$subject)) {
    print(paste0('Subject ', subject))
    ds <- d[d$subject==subject, mask]
    
    # Perform ground truth PCA
    rmat_gt <- prcomp(ds)$rotation
    eigens_gt <- eigen(cov(ds))$values
    matches_gt <- get_matches(rmat_gt)
    
    # On each iteration: 
      # store one composite score for rotation matrix
      # store eigenvalues for all PCs
    subject_result <- list()
    
    # Perform bootstrap resampling
    for (sim in 1:Nsims) {
      subject_result <- bootstrap_simulation(ds, subject_result)
    }
    result <- rbind(result, get_composite_result(mask_size, subject, nrow(ds), subject_result))
    
  } # End subject loop
} # End mask loop

# Visualize result
number_spelling <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", 'Ten', 'Eleven', 'Twelve', 'Thirteen')
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
    caption = paste0("Each point reflects ", Nsims, " samples of one subject's data."),
    title = 'Subject-Wise Bootstrapping'
  ) +
  facet_grid(mask_size~metric) + 
  ylim(0, 1) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        strip.background = element_rect(fill=NA))

ggsave('figures/ppt/PCA_bootstrap_subject.png', height=720, width=1280, units='px', dpi=120)

m1 <- lm(mean_rmat_score ~ nrows * mask_size, data = result)
summary(m1)

# Number of rows seems to matter, mask size matters less


### OVERALL BOOTSTRAP PCA ###

Nsims <- 1000
result <- data.frame()

# Iterate over mask size
for (mask_size in seq(3, length(initial_mask), by=2)) {
  mask <- sample(initial_mask, mask_size)
  print(paste0('Mask size: ', mask_size))
  
  # Iterate over proportion sizes of data
  for (proportion in seq(.1, 1, by=.1)) {
    print(paste0('Proportion: ', proportion))
    
    # Apply mask and grab a proportion
    ds <- d[sample(nrow(d), size=nrow(d)*proportion, replace=FALSE),mask]
    
    # Compute ground truth
    rmat_gt <- prcomp(ds)$rotation
    eigens_gt <- eigen(cov(ds))$values
    matches_gt <- get_matches(rmat_gt)
    
    prop_result <- list()
    
    for (sim in 1:Nsims) {
      prop_result <- bootstrap_simulation(ds, prop_result)  
    }
    result <- rbind(result, get_composite_result(mask_size, proportion, nrow(ds), prop_result))
  }
}

colnames(result)[colnames(result) == 'subject'] <- 'proportion'

# Visualize result
min_mask <- min(result$mask_size)
max_mask <- max(result$mask_size)

result %>% 
  gather(metric, value, mean_rmat_score, mean_eigen_score) %>% 
  mutate(mask_size = paste(number_spelling[mask_size], 'Items'),
         nrows = round(nrows * proportion)) %>% 
  mutate(metric = recode(metric, `mean_eigen_score` = 'Mean Eigenvalue Score',
                         `mean_rmat_score` = 'Mean PC Recovery Score')) %>% 
  mutate(mask_size = factor(mask_size, levels = paste(number_spelling[min_mask:max_mask], 'Items'))) %>% 
  ggplot(aes(x = proportion, y = value)) + 
  geom_point() + 
  #geom_jitter(width = .1) + 
  geom_smooth(method='lm', color='steelblue', fill = 'steelblue', alpha=.4) +
  labs(
    x = 'Proportion of Rows (sampled with replacement)',
    y = 'Score',
    caption = paste0("Each point reflects ", Nsims, " samples from one proportion size of data."),
    title = 'Bootstrapping on Entire Dataset'
  ) +
  facet_grid(mask_size~metric) + 
  scale_x_continuous(breaks = seq(.1, 1, by = .1), labels = seq(.1, 1, by=.1)) + 
  scale_y_continuous(breaks = seq(0, 1, by = .2), labels= seq(0, 1, by = .2), limits = c(0, 1.2)) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        strip.background = element_rect(fill=NA))

ggsave('figures/ppt/PCA_bootstrap_overall.png', height=720, width=1280, units='px', dpi=120)

## WORKING SPACE

true_eigen <- prop_result[['true_eigens']][1]
eigen_dist <- data.frame(eigen_dist = prop_result[['eigen_dist']][['PC1']])
cis <- quantile(eigen_dist$eigen_dist, probs = c(.025, .975))

eigen_dist %>% 
  ggplot(aes(x = eigen_dist)) + 
  geom_histogram(color='black', fill='steelblue') + 
  geom_vline(xintercept = true_eigen, color = 'gold', size = 1.5) + 
  geom_vline(xintercept = cis[1], color = 'red') + 
  geom_vline(xintercept = cis[2], color = 'red') + 
  annotate('text', x = true_eigen-.02, y = 85, label = 'True\nEigenvalue', color = 'gold', size=3, hjust=1) +
  annotate('text', x = cis[1]-.02, y = 20, label = '95% CIs', color = 'red', size=3, hjust=1) +
  labs(
    x = 'Eigenvalue distribution over 1000 simulations',
    y = 'Frequency'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank())

ggsave('figures/ppt/eigen_dist.png', height=500, width=500, units='px', dpi=120)


mask <- c('att', 'run', 'eng', 'self')
ds <- d[sample(nrow(d), replace=TRUE),mask]
rot <- prcomp(ds)$rotation
rot <- data.frame(apply(rot, MARGIN=2, FUN = round, digits =2))
knitr::kable(rot, format='html')

