library(tidyverse)

csvs <- list.files('data/behavioral_data/csv')
d <- data.frame()

repair_missing <- function(d, data) {
  # If d has more columns than data
  missing_cols <- colnames(d)[!colnames(d) %in% colnames(data)]
  for (col in missing_cols) {
    data[col] <- NA
  }
  return(data)
}

for (file in csvs) {
  count <- count + 1
  params <- unlist(strsplit(file, '_'))
  subject <- params[3]
  run <- params[4]
  data <- read.csv(paste0('data/behavioral_data/csv/', file))
  data['subject'] <- subject
  data['run'] <- run
  # Catch missing data
  d <- tryCatch({
    rbind(d, data)
  }, error = function(e) {
    data <- repair_missing(d, data)
    rbind(d, data)
  })
}

# Move subject and run to first columns
d <- cbind(d[,c('subject', 'run')], d[, 1:(ncol(d)-2)])
# Save data
write.csv(d, 'data/behavioral_data/MW_EEG_behavioral.csv', row.names=FALSE)

# Summarize missing data
missing <- apply(d, MARGIN=2, FUN=function(x) sum(is.na(x)))
missing <- data.frame(variables=names(missing), missing_proportion = missing/nrow(d))
missing %>% 
  ggplot(aes(x = reorder(variables, missing_proportion), y = missing_proportion)) + 
  geom_bar(stat='identity') + 
  labs(
    x = 'Variable',
    y = 'Proportion Data Missing'
  ) + 
  ylim(0, .05) + 
  coord_flip() + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 7))

ggsave('scripts/preprocessing/proportion_behavioral_missing.png', height = 1080, width = 1920, units = 'px', dpi=230)





