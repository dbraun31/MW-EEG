rm(list=ls())
library(tidyverse)
library(ggraph)
source('scripts/helpers/normalize.r')
source('scripts/helpers/plot_rotation.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')

items <- c('eng', 'mvmt', 'delib', 'image', 'ling')

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

for (subject in d$subject) {
  pca_data <- d[d$subject==subject, colnames(d) != 'subject']
  subject_pcas[[paste0('subject', subject)]] <- prcomp(pca_data)$rotation[,1:3]
}

# Compute distance and cluster

distance_matrix <- matrix(0, nrow=length(subject_pcas), ncol=length(subject_pcas))

for (i in 1:length(subject_pcas)) {
  for (j in 1:length(subject_pcas)) {
    # Frobenius norm of difference between two matrices
    distance_matrix[i,j] <- norm(subject_pcas[[i]] - subject_pcas[[j]], type='F')
  }
}

# Cluster and plot dendrogram
hc <- hclust(dist(distance_matrix))
dendrogram <- as.dendrogram(hc)

# Create a plot using ggraph
ggraph(dendrogram, layout = "dendrogram") +
  geom_edge_elbow() +  # Customize edge appearance
  geom_node_text(aes(label = label), hjust = 0, size = 6) +  # Add labels
  theme_bw() +  # Customize the theme
  theme(text = element_text(size = 12),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave('figures/pca_subjects_dendro.png', height=1000, width=1000, unit='px', dpi=150)

# Visualize similarities
plot_rotation_subject(subject_pcas, c(3, 4, 5, 8))

ggsave('figures/pca_subjects_rotation.png', height=1000, width=1000, unit='px', dpi=150)
