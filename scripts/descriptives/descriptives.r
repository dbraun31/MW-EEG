rm(list=ls())
library(tidyverse)
source('scripts/helpers/drop_suffix.r')

# Import data 
d <- read.csv('data/behavioral_data/MW_EEG_behavioral.csv')


# Visualize not collapsed across subjects
d %>% 
  gather(probe, response, att:conf) %>% 
  group_by(probe) %>% 
  mutate(response_m = mean(response), y = 0) %>% 
  ungroup() %>% 
  ggplot(aes(x = response, y=y)) +
  geom_jitter(height = .4, alpha = .3) +
  facet_wrap(~probe) + 
  ylim(-10, 10)

# too messy



# Visualize collapsed across subjects
d %>% 
  gather(probe, response, att:conf) %>% 
  group_by(subject, probe) %>% 
  summarize(response = mean(response)) %>% 
  group_by(probe) %>% 
  mutate(response_m = mean(response), y = 0) %>% 
  ungroup() %>% 
  mutate(probe = reorder(probe, response_m)) %>% 
  ggplot(aes(x = response, y=y)) +
  geom_jitter(height = .4, alpha = .6) +
  geom_segment(aes(x = response_m, xend = response_m, y = -4, yend = 4), color = 'steelblue', size = 2, alpha=.8) +
  facet_grid(probe~.) + 
  labs(
    x = 'Response',
    y = '',
    caption='Each data point reflects a subject average. Blue line is overall average.'
  ) +
  ylim(-10, 10) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill=NA))

ggsave('figures/probe_descriptives.png', height=800, width=800, units='px', dpi=120)

# Observation counts by subject
d %>% 
  group_by(subject) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = count)) +
  geom_histogram(color='black', fill='steelblue') + 
  labs(
    x = 'Number of observations',
    y = 'Frequency',
    title = 'Histogram of observations per subject'
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 16))
ggsave('figures/observations_per_subject.png', height=800, width=800, units='px', dpi=96)
