rm(list=ls())
library(tidyverse)
library(scales)
source('scripts/helpers/drop_suffix.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')
# Use dimensions used in past literature
mask <- c('past', 'fut', 'self', 'ppl', 'aff', 'att')

# CONFIDENCE FILTER
# Presently removing below threshold 75
# But this should be revisited
d <- d[d$conf >= 75,]


# NORMALIZE DATA
# Scale subject-wise
subject_scale <- TRUE

# Should also normalize *within* probe

custom_scale <- function(response, response_m, response_sd) {
  # Check to ensure there's a non zero sd
  scaled <- (response - response_m) / response_sd
  out <- ifelse(response_sd != 0 & !is.na(response_sd), scaled, response_m)
  return(out)
}

if (subject_scale) {
  d <- d %>% 
    mutate(id = 1:(nrow(d))) %>% 
    gather(probe, response, att:conf) %>% 
    group_by(subject, probe) %>% 
    mutate(response_m = mean(response), response_sd = sd(response)) %>% 
    ungroup() %>% 
    mutate(response_sc = custom_scale(response, response_m, response_sd)) %>% 
    select(-response, -response_m, -response_sd) %>% 
    rename(response = response_sc) %>% 
    spread(probe, response) %>% 
    select(-run, -id) 
  pca <- prcomp(d[,mask])
  cov_matrix <- cov(d[,mask])
} else {
  pca <- prcomp(d[,mask], scale. = TRUE)
  cov_matrix <- cov(scale(d[,mask]))
}


#### THOUGHT CONTENT ####
result <- data.frame(factor = 1:(ncol(pca$rotation)), eigenvalue = eigen(cov_matrix)$values)

# Scree
importance <- summary(pca)$importance
result %>% 
  ggplot(aes(x = factor, y = eigenvalue, group = 1)) + 
  geom_hline(yintercept=1, linetype='dotted') +
  geom_line(size = 1.5, color='steelblue') + 
  geom_point(size = 2.5) + 
  labs(
    title = 'Scree plot',
    x = 'Number of Factors',
    y = 'Eigenvalue'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.8, .5),
        text=element_text(size = 25))

ggsave('scripts/dim-reduction/scree_plot.png', height=1000, width=1000, units='px', dpi=120)

# Rotation
orange <- brewer_pal(type='div')(9)[2]
blue <- brewer_pal(type='div')(9)[8]
pca$rotation %>% 
  as.data.frame(.) %>% 
  mutate(item = rownames(.)) %>% 
  #select(item, PC1:PC3) %>% 
  gather(factor, loading, PC1:PC5) %>% 
  ggplot(aes(x = factor, y = item, fill = loading)) + 
  geom_tile() + 
  geom_text(aes(label=round(loading, 3)), size = 7) +
  scale_fill_gradient2(low = blue, high = orange) + 
  labs(
    x = 'Factor',
    y = 'Thought Probe',
    fill = 'Loading\nScore'
  ) +
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid=element_blank(),
        text=element_text(size=25),
        legend.position='none')

ggsave('scripts/dim-reduction/pca_result.png', height=1000, width=1000, dpi=120, units='px')


### THOUGHT DYNAMICS ###
dynam_mask <- !colnames(d) %in% c('subject', 'conf', mask) 
pca <- prcomp(d[, dynam_mask])
cov_matrix <- cov(d[,dynam_mask])
result <- data.frame(factor = 1:(ncol(pca$rotation)), eigenvalue = eigen(cov_matrix)$values)


# Scree
importance <- summary(pca)$importance
result %>% 
  ggplot(aes(x = factor, y = eigenvalue, group = 1)) + 
  geom_hline(yintercept=1, linetype='dotted') +
  geom_line(size = 1.5, color='steelblue') + 
  geom_point(size = 2.5) + 
  labs(
    title = 'Scree plot',
    x = 'Number of Factors',
    y = 'Eigenvalue'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.8, .5),
        text=element_text(size = 25))

ggsave('scripts/dim-reduction/scree_plot.png', height=1000, width=1000, units='px', dpi=120)

# Rotation
orange <- brewer_pal(type='div')(9)[2]
blue <- brewer_pal(type='div')(9)[8]
pca$rotation %>% 
  as.data.frame(.) %>% 
  mutate(item = rownames(.)) %>% 
  #select(item, PC1:PC3) %>% 
  gather(factor, loading, PC1:PC5) %>% 
  ggplot(aes(x = factor, y = item, fill = loading)) + 
  geom_tile() + 
  geom_text(aes(label=round(loading, 3)), size = 7) +
  scale_fill_gradient2(low = blue, high = orange) + 
  labs(
    x = 'Factor',
    y = 'Thought Probe',
    fill = 'Loading\nScore'
  ) +
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid=element_blank(),
        text=element_text(size=25),
        legend.position='none')

ggsave('scripts/dim-reduction/pca_result.png', height=1000, width=1000, dpi=120, units='px')