rm(list = ls())
library(tidyverse)

group <- 'eeg'
#group <- 'fmri-eeg'

if (group == 'eeg') {
  path <- "data/behavioral_data/MW_EEG_behavioral_full.csv"
} else if (group == 'fmri-eeg') {
  path <- 'scripts/descriptives/fmri-eeg_data/pilot.csv'
}
d <- read.csv(path)

if (group == 'eeg') {
  # Drop subjects with < 40 trials
  N_start <- length(unique(d$subject))
  keep <- d %>% 
    group_by(subject) %>% 
    summarize(count = n()) %>% 
    filter(count >= 40) %>% 
    pull(subject)
  d <- d[d$subject %in% keep,]
  print(paste0('Dropping ', N_start - length(unique(d$subject)), ' of ', N_start, ' subjects for < 40 trials'))
}

# Keep only relevant columns
d <- select(d, subject, run, arou_response, arou_onset)

# Number of subjects
N <- length(unique(d$subject))

# Missing arousal responses
print(sum(is.na(d$arou_response)) / nrow(d))
print(d[is.na(d$arou_response), ])
d <- d[!is.na(d$arou_response),]

# Make subject-level summaries
subject_summary <- d %>%
    group_by(subject) %>%
    summarize(trial_count = n(),
              arousal_mean = round(mean(arou_response), 2),
              arousal_sd = round(sd(arou_response), 2)) %>%
    select(subject,trial_count, arousal_mean, arousal_sd) 

# Visualize arousal distribution
d %>% 
  group_by(subject) %>% 
  summarize(arou = mean(arou_response)) %>% 
  ggplot(aes(x = arou)) + 
  geom_histogram(color = 'black', fill = 'steelblue', alpha = .8) + 
  xlim(0, 100) + 
  labs(
    x = 'Mean Arousal Rating',
    y = 'Frequency',
    caption = 'Averaged across subjects'
  ) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())
  
  
path <- paste0('scripts/descriptives/arousal_distribution_', group, '.png')
ggsave(path, height = 800, width = 800, units='px', dpi = 100) 


# Look at arousal trends over session

# Compute trial counters
d <- d %>%
  group_by(subject) %>%
  mutate(trial_session = 1:n()) %>%
  ungroup() %>% 
  select(subject, run, trial_session, arou_onset, arou_response)


## CORRELATE ACROSS SESSION ##
library(scales)
cols <- c('black', 
          brewer_pal(palette='Blues')(9)[7],
          brewer_pal(palette='Blues')(9)[5],
          brewer_pal(palette='Blues')(9)[3])
 
sig_levels = c('N.S.', 'p < .05', 'p < .01', 'p < .001')

subs <- d %>%
  group_by(subject) %>%
  summarize(r = round(cor(trial_session, arou_response), 3), 
            p = round(cor.test(trial_session, arou_response)$p.value, 3),
            ci_h = round(cor.test(trial_session, arou_response)$conf.int[2], 3), 
            ci_l = round(cor.test(trial_session, arou_response)$conf.int[1], 3)) %>%
  mutate(issig = ifelse(p < .001, 'p < .001', 
                        ifelse(p < .01, 'p < .01', 
                               ifelse(p < .05, 'p < .05', 'N.S.')))) %>%
  mutate(issig = factor(issig, levels = sig_levels))


grand <- subs %>% 
  summarize(r_mean = round(mean(r), 3), r_se = sd(r) / sqrt(N)) %>% 
  mutate(ci_h = round(1.94 * r_se, 3), ci_l = round(-1.94 * r_se, 3))

subs %>% 
  ggplot(aes(x = reorder(subject, r), y = r)) +
  geom_hline(yintercept=0, linetype='dotted') + 
  geom_point(aes(color=issig)) + 
  geom_errorbar(aes(ymin = ci_l, ymax = ci_h, color = issig, linetype=issig), width = 0) + 
  labs(
    title= 'Session-level correlation between time in session and arousal',
    x = 'Subject',
    y = 'Correlation (r)',
    linetype = '',
    color = '',
    caption = 'Horizontal lines reflect 95% CIs'
  ) +
  coord_flip() +
  annotate('text', x = 26, y = -.8, label = paste0('Overall:\nMean r: ', 
                                                   grand$r_mean, '\nCIs: [', grand$ci_l, ', ',
                                                   grand$ci_h, ']'), hjust=0) +
  scale_color_manual(values = c('N.S.'=cols[1], 'p < .05'=cols[2], 'p < .01'=cols[3], 'p < .001'=cols[4])) + 
  ylim(-1, 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.17, .9))

path <- paste0('scripts/descriptives/session_correlation_', group, '.png')
ggsave(path, height = 800, width = 800, units='px', dpi = 100) 

## Histogram of r values ##
subs %>% 
  ggplot(aes(x = r)) + 
  geom_histogram(color = 'black', fill = 'steelblue', alpha = .8, bins = 15) + 
  labs(
    title = 'Distribution of within-session arousal correlations', 
    x = 'Subject-wise correlations (r)',
    y = 'Frequency'
  ) + 
  xlim(-1, 1) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())

path <- paste0('scripts/descriptives/session_correlation_histogram_', group, '.png')
ggsave(path, height = 800, width = 800, units='px', dpi = 100) 

# Concatenate to summary table
subject_summary <- subs %>% 
  rename(arousal_session_r = r,
         arousal_session_p = p,
         arousal_session_ci_h = ci_h,
         arousal_session_ci_l = ci_l) %>% 
  select(-issig) %>% 
  inner_join(subject_summary) %>% 
  select(subject, trial_count, arousal_mean, arousal_sd, arousal_session_r:arousal_session_ci_l) 

path <- paste0('scripts/descriptives/subject_summary_', group, '.csv')
write.csv(subject_summary, path, row.names = FALSE)


### CORRELATE WITHIN RUN ###

cols <- c('black', 
          brewer_pal(palette='Blues')(9)[7],
          brewer_pal(palette='Blues')(9)[5],
          brewer_pal(palette='Blues')(9)[3])

sig_levels = c('N.S.', 'p < .05', 'p < .01', 'p < .001')

subs <- d %>%
  group_by(subject) %>%
  summarize(r = round(cor(arou_onset, arou_response), 3), 
            p = round(cor.test(arou_onset, arou_response)$p.value, 3),
            ci_h = round(cor.test(arou_onset, arou_response)$conf.int[2], 3), 
            ci_l = round(cor.test(arou_onset, arou_response)$conf.int[1], 3)) %>%
  mutate(issig = ifelse(p < .001, 'p < .001', 
                        ifelse(p < .01, 'p < .01', 
                               ifelse(p < .05, 'p < .05', 'N.S.')))) %>%
  mutate(issig = factor(issig, levels = sig_levels))


grand <- subs %>% 
  summarize(r_mean = round(mean(r), 3), r_se = sd(r) / sqrt(N)) %>% 
  mutate(ci_h = round(1.94 * r_se, 3), ci_l = round(-1.94 * r_se, 3))

subs %>% 
  ggplot(aes(x = reorder(subject, r), y = r)) +
  geom_hline(yintercept=0, linetype='dotted') + 
  geom_point(aes(color=issig)) + 
  geom_errorbar(aes(ymin = ci_l, ymax = ci_h, color = issig, linetype=issig), width = 0) + 
  labs(
    title= 'Run-level correlation between time in run and arousal',
    x = 'Subject',
    y = 'Correlation (r)',
    linetype = '',
    color = '',
    caption = 'Horizontal lines reflect 95% CIs'
  ) +
  coord_flip() +
  annotate('text', x = 23, y = -.8, label = paste0('Overall:\nMean r: ', 
                                                   grand$r_mean, '\nCIs: [', grand$ci_l, ', ',
                                                   grand$ci_h, ']'), hjust=0) +
  scale_color_manual(values = c('N.S.'=cols[1], 'p < .05'=cols[2], 'p < .01'=cols[3], 'p < .001'=cols[4])) + 
  ylim(-1, 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.17, .9))

path <- paste0('scripts/descriptives/run_correlation_', group, '.png')
ggsave(path, height = 800, width = 800, units='px', dpi = 100) 

## Histogram of r values ##
subs %>% 
  ggplot(aes(x = r)) + 
  geom_histogram(color = 'black', fill = 'steelblue', alpha = .8, bins = 15) + 
  labs(
    title = 'Distribution of within-run arousal correlations', 
    x = 'Subject-wise correlations (r)',
    y = 'Frequency'
  ) + 
  xlim(-1, 1) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())

path <- paste0('scripts/descriptives/run_correlation_histogram_', group, '.png')
ggsave(path, height = 800, width = 800, units='px', dpi = 100) 

# Concatenate to summary table
subject_summary <- subs %>% 
  rename(arousal_run_r = r,
         arousal_run_p = p,
         arousal_run_ci_h = ci_h,
         arousal_run_ci_l = ci_l) %>% 
  select(-issig) %>% 
  inner_join(subject_summary) %>% 
  select(subject, trial_count, arousal_mean, arousal_sd, starts_with('arousal')) 
   

path <- paste0('scripts/descriptives/subject_summary_', group, '.csv')
write.csv(subject_summary, path, row.names = FALSE)
