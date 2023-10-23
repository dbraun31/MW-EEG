rm(list=ls())
library(tidyverse)
library(ggraph)
source('scripts/helpers/normalize.r')
source('scripts/helpers/plotters.r')
source('scripts/helpers/computers.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')

# Content or movement items
content <- TRUE

if (content) {
  items <- c('att', 'past', 'fut', 'self', 'ppl', 'arou', 'aff', 'image', 'ling')
} else {
  items <- c('eng', 'mvmt', 'delib')
}

# Keep only subjects with 50 observations
subject_mask <- d %>% 
  group_by(subject) %>% 
  summarize(count = n()) %>% 
  filter(count >= 50) %>% 
  pull(subject)


d <- d[d$subject %in% subject_mask, ] 

# Drop low confidence (< 75) responses
d <- d[d$conf > 75, c('subject', items)]

# Normalize
d <- normalize_subject_item(d)

# Run PCAs
subject_pcas <- list()
Nfactors <- min(length(mask), 6)

for (subject in d$subject) {
  pca_data <- d[d$subject==subject, colnames(d) != 'subject']
  subject_pcas[['rotations']][[paste0('subject', subject)]] <- prcomp(pca_data)$rotation[,1:Nfactors]
  subject_pcas[['eigens']][[paste0('subject', subject)]] <- eigen(cov(pca_data))$values[1:Nfactors]
}

# Compute distance and cluster

distance_matrix <- matrix(0, nrow=length(subject_pcas[['rotations']]), ncol=length(subject_pcas[['rotations']]))

for (i in 1:length(subject_pcas[['rotations']])) {
  for (j in 1:length(subject_pcas[['rotations']])) {
    # Frobenius norm of difference between two matrices
    # distance_matrix[i,j] <- norm(subject_pcas[[i]] - subject_pcas[[j]], type='F')
    # Mean Euclidean distance between each column of two matrices
    # Could expand this to weight the average by the eigenvalues
    distance_matrix[i,j] <- weighted_distance(subject_pcas[['rotations']][[i]] - subject_pcas[['rotations']][[j]],
                                              e1=subject_pcas[['eigens']][[i]], e2=subject_pcas[['eigens']][[j]])
  }
}

# Cluster and plot dendrogram
hc <- hclust(dist(distance_matrix))
dendrogram <- as.dendrogram(hc)

ggraph(dendrogram, layout = "dendrogram") +
  geom_edge_elbow() +  # Customize edge appearance
  geom_node_text(aes(label = label), hjust = 0, size = 6) +  # Add labels
  theme_bw() +  # Customize the theme
  theme(text = element_text(size = 12),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave('figures/pca_subjects_dendro.png', height=1000, width=1000, unit='px', dpi=150)

## VISUALIZE WITH HEAT MAP ##
#plot_rotation_subject(subject_pcas[['rotations']], c(1, 2))

ggsave('figures/pca_subjects_rotation.png', height=1000, width=1000, unit='px', dpi=150)


## VISUALIZE WITH WORD CLOUD ##
plot_word_cloud(subject_pcas, c(5,6,1,7))

