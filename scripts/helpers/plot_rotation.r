library(scales)

decode_subject <- function(subject_pcas, idx) {
  # Takes in subject_pcas and index number of subjects of interest
  # Returns subject number
  return(names(subject_pcas[idx]))
}

plot_rotation_subject <- function(subject_pcas, subject_idxs) {
  # Takes in subject pcas and subjects to plot
  # Converts to df and plots faceted by subject
  # Fixed at five items for now
  
  d <- data.frame()
  
  for (subject_idx in subject_idxs) {
    rot <- data.frame(subject_pcas[[subject_idx]])
    rot$subject <- subject_idx
    rot$item <- rownames(rot)
    d <- rbind(d, rot)
  }
  
  
  # Plot
  orange <- brewer_pal(type='div')(9)[2]
  blue <- brewer_pal(type='div')(9)[8]
  d %>% 
    #select(item, PC1:PC3) %>% 
    gather(factor, loading, PC1:PC3) %>% 
    mutate(subject = subject) %>% 
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
