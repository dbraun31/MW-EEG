library(scales)
library(ggwordcloud)
source('scripts/helpers/item_names_dict.r')

### HEAT MAP OF ROTATION MATRIX ###

decode_subject <- function(subject_pcas, idx) {
  # Takes in subject_pcas and index number of subjects of interest
  # Returns subject number
  return(names(subject_pcas[idx]))
}

rotations_to_df <- function(subject_pcas, subject_idxs) {
  
  d <- data.frame()
  for (subject_idx in subject_idxs) {
    rot <- data.frame(subject_pcas[['rotations']][[subject_idx]])
    eigens <- subject_pcas[['eigens']][[subject_idx]]
    rot <- data.frame(subject = subject_idx, item = item_names_dict[rownames(rot)], rot)
    d <- rbind(d, rot)
  }
  return(d)
}

eigens_to_df <- function(subject_pcas, subject_idxs) {
  # Returns a df: | subject | PC | eigens |
  
  d <- data.frame()
  for (subject_idx in subject_idxs) {
    eigens <- subject_pcas[['eigens']][[subject_idx]]
    d <- rbind(d, data.frame(subject = subject_idx, PC = paste0('PC', 1:(length(eigens))), eigens=eigens))
  }
  return(d)
  
}

plot_rotation_subject <- function(subject_pcas, subject_idxs) {
  # Takes in subject pcas and subjects to plot
  # Converts to df and plots faceted by subject
  # Fixed at five items for now
  
  d <-  rotations_to_df(subject_pcas, subject_idxs)
  
  # Plot
  orange <- brewer_pal(type='div')(9)[2]
  blue <- brewer_pal(type='div')(9)[8]
  d %>% 
    #select(item, PC1:PC3) %>% 
    gather(factor, loading, PC1:(ncol(d))) %>% 
    #mutate(subject = subject) %>% 
    ggplot(aes(x = factor, y = item, fill = loading)) + 
    geom_tile() + 
    geom_text(aes(label=round(loading, 3)), size = 5) +
    scale_fill_gradient2(low = blue, high = orange) + 
    labs(
      x = 'Factor',
      y = 'Experience Sampling Item',
      fill = 'Loading\nScore'
    ) +
    facet_grid(subject~.) + 
    theme_bw() + 
    theme(axis.ticks = element_blank(),
          panel.grid=element_blank(),
          text=element_text(size=20),
          legend.position='none',
          strip.background = element_rect(fill=NA))
  
}

### WORD CLOUD OF ROTATION MATRIX ###

# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

plot_word_cloud <- function(subject_pcas, subject_idxs) {

  d <- rotations_to_df(subject_pcas, subject_idxs)
  eigen_df <- eigens_to_df(subject_pcas, subject_idxs)
  
  d %>% 
    gather(PC, loading, PC1:(ncol(d))) %>% 
    inner_join(eigen_df) %>% 
    ggplot(aes(label=item, size = abs(loading), color = loading)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 30) +
    scale_color_gradientn(colors = rev(brewer_pal(type='div')(9))) + 
    facet_grid(subject~PC) + 
    theme_bw() + 
    theme(strip.background = element_rect(fill=NA))
  }
