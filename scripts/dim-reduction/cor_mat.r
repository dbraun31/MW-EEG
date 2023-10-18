library(scales)
library(tidyverse)
rm(list=ls())
source('scripts/helpers/drop_suffix.r')

d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')

# Average across subjects
d <- d[complete.cases(d), c('subject', 'run', grep('*_response$', colnames(d), value=TRUE))]
colnames(d)[3:(ncol(d))] <- drop_suffix(d[3:(ncol(d))])
d <- d %>% 
  gather(dimension, response, att:conf) %>% 
  group_by(subject, dimension) %>% 
  summarize(response = mean(response)) %>% 
  spread(dimension, response) 
  

# Calculate the correlation matrix
correlation_matrix <- cor(d[,!colnames(d) %in% 'subject'])

# Visualize correlation matrix
# put significance markers on here at some point
high <- brewer_pal(type='div')(9)[2]
low <- brewer_pal(type='div')(9)[8]
correlation_matrix %>% 
  as.data.frame() %>% 
  mutate(var1 = names(.)) %>% 
  gather(var2, cor, aff:self) %>% 
  mutate(highlight = ifelse(var1 > var2, round(cor, 2), '')) %>% 
  ggplot(aes(x = var1, y = var2, fill = cor)) + 
  geom_tile() + 
  geom_text(aes(label=highlight), size = 7) + 
  scale_fill_gradient2(low=low, high=high, mid='white', midpoint=0, limits=c(-.8,.8)) +
  labs(
    x = '',
    y = '',
    fill = 'Correlation\nStrength'
  ) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1),
        text = element_text(size=25),
        legend.position='none')

ggsave('scripts/dim-reduction/correlation_matrix.png', height=720, width=1280, units='px', dpi=96)
