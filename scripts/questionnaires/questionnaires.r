rm(list=ls())
library(tidyverse)
library(readxl)
library(scales)
library(gtools)
library(ggrepel)

d <- read_excel('data/surveys/surveys.xlsx')
groups <- read.csv('scripts/dim-reduction/subject-clusters_fifty.csv')

d <- d %>% 
  rename(subject = subj_id) %>% 
  select(subject, ends_with('_Score')) %>% 
  filter(subject %in% groups$subject) %>% 
  inner_join(groups)

colnames(d) <- gsub('_Score', '', colnames(d))

N <- length(unique(d$subject))

blue <- brewer_pal(type='div')(9)[7]
orange <- brewer_pal(type='div')(9)[2]

# All surveys
d %>% 
  gather(dimension, score, `I_DSM-TR`:STAI) %>% 
  group_by(dimension) %>% 
  mutate(max_score = max(score)) %>% 
  ungroup() %>% 
  mutate(score_n = score / max_score) %>% 
  group_by(cluster, dimension) %>% 
  summarize(mean = mean(score_n), se = sd(score_n) / sqrt(N)) %>% 
  mutate(cluster = recode(cluster, `1` = 'Self', `2` = 'Other')) %>% 
  ggplot(aes(x = dimension, y = mean, fill = factor(cluster))) + 
  geom_bar(stat = 'identity', color = 'black', position=position_dodge(width=.9)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position=position_dodge(width=.9), width = .5) +
  labs(
    x = 'Dimension',
    y = 'Normalized Average Score',
    caption = paste0('N = ', N),
    fill = 'Group'
  ) +
  coord_flip() + 
  scale_fill_manual(values = c('Self' = blue, 'Other' = orange)) + 
  theme_bw() +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.85, .85))
  
ggsave('figures/surveys/survey_forty_normalized.png', 
       height = 1080, width = 1920/2, units = 'px', dpi = 96)

t.test(d[d$cluster==1,]$RRS, d[d$cluster==2,]$RRS, paired=FALSE)
t.test(d[d$cluster==1,]$STAI, d[d$cluster==2,]$STAI, paired=FALSE)
t.test(d[d$cluster==1,]$WHODAS, d[d$cluster==2,]$WHODAS, paired=FALSE)
t.test(d[d$cluster==1,]$`MW-S`, d[d$cluster==2,]$`MW-S`, paired=FALSE)
t.test(d[d$cluster==1,]$PHQ9, d[d$cluster==2,]$PHQ9, paired=FALSE)

# Mean affect and questionnaires
es <- read.csv('data/behavioral/MW_EEG_behavioral.csv')
d <- es %>% 
  group_by(subject) %>% 
  summarize(aff = mean(aff)) %>% 
  inner_join(d) 

corrs <- data.frame()

iter_cols <- colnames(d)[!colnames(d) %in% c('subject', 'cluster', 'aff')]
for (col in iter_cols) {
  m <- cor.test(d[['aff']], d[[col]])
  r <- m$estimate
  p <- m$p.value
  corrs <- rbind(corrs, data.frame(survey = col, r = r, p = p))
}

corrs$stat <- ifelse(corrs$p < .05, paste0(round(corrs$r, 3), '*'), round(corrs$r, 3))

d %>% 
  select(-cluster) %>% 
  gather(survey, response, `I_DSM-TR`:STAI) %>% 
  group_by(survey) %>% 
  mutate(max = max(response)) %>% 
  ungroup() %>% 
  mutate(response_n = response / max) %>% 
  ggplot(aes(x = aff, y = response_n)) + 
  geom_point(alpha = .6) + 
  geom_smooth(method='lm') + 
  geom_text_repel(data = corrs, x = 50, y = 1, aes(label = paste0('r = ', stat)), color = 'blue') + 
  facet_wrap(~survey) + 
  labs(
    x = 'Mean affect rating',
    y = 'Normalized response',
    caption = paste0('N = ', N)
  ) +
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = NA),
        text = element_text(size = 18))

ggsave('figures/surveys/survey_affect_fifty.png', 
       height = 1080, width = 1920/2, units = 'px', dpi = 96)



## RRS Subscales ##

d <- read_excel('data/surveys/surveys.xlsx')
d <- d %>% 
  select(subj_id, starts_with('rrs')) %>% 
  select(-rrs_timestamp, -RRS_Score, -rrs_complete) %>% 
  rename(subject = subj_id) %>% 
  inner_join(groups) 

d %>% 
  gather(rrs, response, rrs1:rrs22) %>% 
  group_by(rrs, cluster) %>% 
  summarize(mean = mean(response), se = sd(response) / sqrt(N)) %>% 
  mutate(cluster = factor(cluster),
         rrs = factor(rrs)) %>% 
  mutate(rrs = factor(rrs, levels = rev(mixedsort(levels(rrs)))),
         cluster = recode(cluster, `1` = 'Self', `2` = 'Other')) %>% 
  ggplot(aes(x = rrs, y = mean, group=cluster)) + 
  geom_bar(stat='identity', aes(fill = cluster), position=position_dodge(width=.9), color = 'black') +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position=position_dodge(width=.9), width=.5) + 
  labs(
    x = 'RRS Subdimension',
    y = 'Average response',
    fill = 'Group',
    caption = paste0('N = ', N)
  ) +
  coord_flip() + 
  scale_fill_manual(values = c('Self' = blue, 'Other' = orange)) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(.9, .5),
        text = element_text(size = 18))

  
ggsave('figures/surveys/rrs_fifty.png', 
       height = 1080, width = 1920/2, units = 'px', dpi = 96)

