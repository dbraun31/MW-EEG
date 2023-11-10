rm(list=ls())
library(tidyverse)
library(ggraph)
source('scripts/helpers/plotters.r')
source('scripts/helpers/computers.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')

## CONTENT OR MOVEMENT ITEMS ##
# c('rumination', 'content', 'dynamic')
item_set <- 'rumination'

if (item_set=='content') {
  items <- c('past', 'fut', 'self', 'ppl', 'aff')
  #items <- c('att', 'past', 'fut', 'self', 'ppl', 'arou', 'aff', 'image', 'ling')
} else if (item_set == 'dynamic') {
  items <- c('eng', 'mvmt', 'delib')
} else if (item_set == 'rumination') {
  items <- c('past', 'fut', 'self', 'ppl', 'aff', 'mvmt')
}

# Keep only relevant items
d <- d[, c('subject', items)]

# Drop missing data
d <- d[complete.cases(d),]

## FILTER 50 TRIALS ONLY OR >= 40 ##
trimming <- 'fifty'
if (trimming == 'fifty') {
  filter_criterion <- 50
} else if (trimming == 'forty') {
  filter_criterion <- 40
}

# Keep only subjects with determined number of observations
subject_mask <- d %>% 
  group_by(subject) %>% 
  summarize(count = n()) %>% 
  filter(count >= filter_criterion) %>% 
  pull(subject)

d <- d[d$subject %in% subject_mask, ] 

# Drop low confidence (< 75) responses
#d <- d[d$conf > 75, c('subject', items)]


# Normalize
d <- normalize_subject_item(d)

# Run PCAs
subject_pcas <- list()
Nfactors <- min(length(items), 6)

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
## note, subject numbers coming out of this do *not* match up with subject numbers
## in dataset. rather, they correspond to index in the subject_pcas list.

ggraph(dendrogram, layout = "dendrogram") +
  geom_edge_elbow() +  # Customize edge appearance
  geom_node_text(aes(label = label), hjust = 0, vjust= -1, size = 5) +  # Add labels
  labs(
    x = '',
    y = ''
  ) +
  theme_bw() +  # Customize the theme
  coord_flip() + 
  theme(text = element_text(size = 12),
        #axis.text = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())  

## Number of groups to extract
Ngroups <- 2
clusters <- cutree(hc, k=Ngroups)

ggsave(paste0('figures/word_clouds/', trimming, '/dendro_', trimming, '.png'), 
              height=720, width=300, unit='px', dpi=96)


## VISUALIZE WITH WORD CLOUD ##
for (group in 1:(Ngroups)) {
  subject_idxs <- which(clusters==group)
  for (half in c('a', 'b')) {
    if (half == 'a') {
      mask <- 1:(round(length(subject_idxs)/2))
    } else {
      mask <- (round(length(subject_idxs)/2)+1):length(subject_idxs)
    }
    plot_word_cloud(subject_pcas, subject_idxs[mask], max_size = 30)
    path <- paste0('figures/word_clouds/', trimming, '/rumination_', trimming, '_', group, half, '.png')
    ggsave(path, height = 1080, width = 1920/2, units = 'px', dpi = 96)
  }
}



### GROUP LEVEL PCA ###

# make subject mapping
subject_mapping <- data.frame(subject_idx = 1:length(unique(d$subject)), subject_id = unique(d$subject))
group_pcas <- list()

for (group in 1:Ngroups) {
  subject_idxs <- which(clusters==group)
  subject_ids <- subject_mapping[subject_mapping$subject_idx %in% subject_idxs,]$subject_id
  data <- d[d$subject %in% subject_ids, !colnames(d) %in% 'subject']
  group_pcas[['rotations']][[paste0('group', group)]] <- prcomp(data)$rotation
  group_pcas[['eigens']][[paste0('group', group)]] <- eigen(cov(data))$values
}

plot_word_cloud(group_pcas, 1:2, max_size = 30)

ggsave(paste0('figures/word_clouds/', trimming, '/groups_', trimming, '.png'), 
       height = 1080, width = 1920/2, units = 'px', dpi = 96)




