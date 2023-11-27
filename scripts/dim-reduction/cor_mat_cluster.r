rm(list=ls())
library(tidyverse)
library(dendextend)
source('scripts/helpers/plotters.r')
source('scripts/helpers/computers.r')

idx_to_subject <- function(idx, d) {
  subjects <- unique(d$subject)
  return(subjects[idx])
}

d <- read.csv('data/behavioral/MW_EEG_behavioral.csv')

# Drop missing data
d <- d[complete.cases(d),]

# Keep only subjects with determined number of observations
subject_mask <- d %>% 
  group_by(subject) %>% 
  summarize(count = n()) %>% 
  filter(count >= 40) %>% 
  pull(subject)

d <- d[d$subject %in% subject_mask, ] 

d <- d[, !colnames(d) %in% c('conf', 'run')]

# Normalize
d <- normalize_subject_item(d)

# Get correlation matrix for each subject
cors <- list()
for (subject in unique(d$subject)) {
  t <- d[d$subject==subject, colnames(d) != 'subject']
  cors[[paste0('sub_', subject)]] <- cor(t)
}

# Compute distances
N <- length(unique(d$subject))
distances <- matrix(0, nrow = N, ncol = N)

frob <- function(m1, m2) {
  return(sqrt(sum((m1 - m2)^2)))
}

for (subject_i in 1:N) {
  for (subject_j in 1:N) {
    distances[subject_i,subject_j] <- frob(cors[[subject_i]], cors[[subject_j]])
  }
  
}

# Cluster
# The ids here are indices of the subject vector
hc <- hclust(dist(distances))
plot(hc)

m1 <- cor(d[d$subject==idx_to_subject(3,d), colnames(d) != 'subject'])
m2 <- cor(d[d$subject==idx_to_subject(28,d), colnames(d) != 'subject'])

plot_cors(m1, m2)

ggsave('figures/cor_cluster/cor_heat.png', height = 720, width = 1280, units = 'px', dpi = 96)

# Group cors
groups <- 4
labels <- cutree(hc, k=groups)

# Plot colored dendro
dendro <- as.dendrogram(hc)
dendro <- color_branches(dendro, k = groups, groupLabels = TRUE)
dendro <- hang.dendrogram(dendro, hang_height=0.1)
p <- plot(rev(dendro), horiz = TRUE)

# Save dendro to file
pdf('figures/2023-11-28_labmeeting/dendro.pdf', height = 1080/92, width = 1920/3/92)
plot(rev(dendro), horiz = TRUE)
dev.off()


cluster_code <- data.frame(subject = unique(d$subject), cluster = labels)
m <- d %>% 
  inner_join(cluster_code) %>% 
  mutate(cluster = paste('Group', cluster)) %>% 
  select(-subject)

cors <- list()

for (group in 1:groups) {
  cors[[paste0('group', group)]] <- cor(m[m$cluster == paste('Group', group), !colnames(m) %in% c('cluster')])
}

plot_cors(cors)
ggsave('figures/2023-11-28_labmeeting/cors.pdf', height=1080/92, width = 1920*(2/3)/92, units = 'in')
  
# Diff group1 vs group2
cors_ <- cors[['group1']] - cors[['group2']]

plot_cors(cors_, title = 'Group 1 - Group 2')

