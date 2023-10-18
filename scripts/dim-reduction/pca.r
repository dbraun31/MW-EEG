rm(list=ls())
library(tidyverse)
library(scales)
source('scripts/helpers/drop_suffix.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')
og_rows <- nrow(d)
d <- d[complete.cases(d),  grep('*_response$', colnames(d), value=TRUE)]
colnames(d) <- drop_suffix(d)
percent_missing <- round((og_rows - nrow(d))/og_rows, 4) * 100
print(paste0(percent_missing, '% of the data was dropped due to missing observations on one or more variables'))

# Use dimensions used in past literature
mask <- c('past', 'fut', 'self', 'ppl', 'aff')

# Run PCA
pca <- prcomp(d[,mask], scale=TRUE)

# Summarize results

# Scree
importance <- summary(pca)$importance
result <- data.frame(factor = 1:(ncol(pca$rotation)), eigenvalue = pca$sdev^2)
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
  select(item, PC1:PC3) %>% 
  gather(factor, loading, PC1:PC3) %>% 
  ggplot(aes(x = factor, y = item, fill = loading)) + 
  geom_tile() + 
  geom_text(aes(label=round(loading, 3)), size = 7) +
  scale_fill_gradient2(low=blue, high=orange) +
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
