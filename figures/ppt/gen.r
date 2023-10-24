rm(list=ls())
library(tidyverse)
## PCA demo

height <- rnorm(100, 65, 4)
weight <- 10 + 2 * height + rnorm(100, 0, 10)
d <- data.frame(height=height, weight=weight)

# Fig dims
height = 400
width = 400


# 00

d %>% 
  ggplot(aes(x = height, y = weight)) + 
  geom_point() + 
  labs(
    x = 'Height (in)',
    y = 'Weight (lbs)'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave('figures/ppt/00.png', height=height, width=width, units='px', dpi=120)

# 01
d <- data.frame(scale(d))

d %>% 
  ggplot(aes(x = height, y = weight)) + 
  geom_point() + 
  labs(
    x = 'Height (z-score)',
    y = 'Weight (z-score)'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave('figures/ppt/01.png', height=height, width=width, units='px', dpi=120)

## 02

m1 <- lm(weight ~ height, data = d)

d %>% 
  ggplot(aes(x = height, y = weight)) + 
  geom_point() + 
  geom_abline(slope = m1$coefficients[2], intercept = 0, color = 'blue') +
  labs(
    x = 'Height (z-score)',
    y = 'Weight (z-score)'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave('figures/ppt/02.png', height=height, width=width, units='px', dpi=120)

## 02a
d %>% 
  ggplot(aes(x = height, y = weight)) + 
  geom_point() + 
  geom_abline(slope = m1$coefficients[2], intercept = 0, color = 'blue') +
  labs(
    x = 'Height (z-score)',
    y = 'Weight (z-score)'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave('figures/ppt/02a.png', height=height, width=width, units='px', dpi=120)

# 02b

d %>% 
  ggplot(aes(x = height, y = weight)) + 
  geom_point() + 
  geom_abline(slope = m1$coefficients[2], intercept = 0, color = 'blue') +
  geom_abline(slope = -(1/m1$coefficients[2]), intercept=0, color='red') + 
  labs(
    x = 'Height (z-score)',
    y = 'Weight (z-score)'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave('figures/ppt/02b.png', height=height, width=width, units='px', dpi=120)

## 03

pca <- prcomp(d)
xmax <- max(data.frame(pca$x)$PC1)
xmin <- min(data.frame(pca$x)$PC1)

pca$x %>% 
  as.data.frame(.) %>% 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point() + 
  ylim(c(xmin, xmax)) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave('figures/ppt/03.png', height=height, width=width, units='px', dpi=120)
