library(abind)
library(scales)
library(tidyverse)
rm(list=ls())
source('scripts/helpers/drop_suffix.r')
d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')
 
# We want to compute correlations and *then* average over subjects 
# Gives us a better sense of trial effects rather than individual differences

# Keep only relevant columns
d <- d[complete.cases(d), c('subject', 'run', grep('*_response$', colnames(d), value=TRUE))]
colnames(d)[3:(ncol(d))] <- drop_suffix(d[,3:(ncol(d))])

# Loop over subjects
cors <- list()
for (s in unique(d$subject)) {
  print(s)
  d_mat <- d[d$subject==s, 3:(ncol(d))]
  cors[[s]] <- cor(d_mat)
}

stacked <- abind(cors, along=3)
mean_cors <- apply(stacked, c(1,2), mean, na.rm=TRUE)
sd_cors <- apply(stacked, c(1,2), sd, na.rm=TRUE)
sd_cors <- sd_cors %>% 
  as.data.frame() %>% 
  mutate(var1 = names(.)) %>% 
  gather(var2, sd_cor, att:conf) 

# Visualize correlation matrix
# put significance markers on here at some point
high <- brewer_pal(type='div')(9)[2]
low <- brewer_pal(type='div')(9)[8]
mean_cors %>% 
  as.data.frame() %>% 
  mutate(var1 = names(.)) %>% 
  gather(var2, mean_cor, att:conf) %>% 
  left_join(sd_cors) %>% 
  mutate(highlight_text = ifelse(var1 > var2, round(mean_cor, 2), round(sd_cor, 2))) %>% 
  mutate(highlight_color = ifelse(var1 > var2, round(mean_cor, 2), 0)) %>% 
  ggplot(aes(x = var1, y = var2, fill = highlight_color)) + 
  geom_tile() + 
  geom_text(aes(label=highlight_text), size = 5) + 
  scale_fill_gradient2(low=low, high=high, mid='white', midpoint=0, limits=c(-.8,.8)) +
  labs(
    x = '',
    y = '',
    fill = 'Correlation\nStrength',
    caption = 'Shaded region reflects average correlation.\nUnshaded region reflects standard deviation.'
  ) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position='none',
        plot.caption = element_text(size = 12))

ggsave('figures/correlation_matrix.png', height=800, width=800, units='px', dpi=96)