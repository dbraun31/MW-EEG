library(scales)
library(ggwordcloud)
source('scripts/helpers/objects.r')

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
    rot <- data.frame(case = subject_idx, item = item_names_dict[rownames(rot)], rot)
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

plot_word_cloud <- function(pcas, idxs, max_size = 30) {

  d <- rotations_to_df(pcas, idxs)
  eigen_df <- eigens_to_df(pcas, idxs)
  
  d %>% 
    # Take only first three PCs
    gather(PC, loading, PC1:PC3) %>% 
    #gather(PC, loading, PC1:(ncol(d))) %>% 
    mutate(case = factor(case, levels = idxs)) %>% 
    ggplot(aes(label=item, size = abs(loading), color = loading)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = max_size) +
    scale_color_gradientn(colors = rev(brewer_pal(type='div')(9))) + 
    facet_grid(case~PC) + 
    theme_bw() + 
    theme(strip.background = element_rect(fill=NA))
  }

### HEAT MAP OF TWO CORRELATION MATRICES

blue <- brewer_pal(type='div')(9)[8]
orange <- brewer_pal(type='div')(9)[2]


plot_cors <- function(cors) {
  # Takes in a list of correlation matrices
  # Plots each in a separate facet
  
  
}


plot_cors <- function(data, title = NULL) {
  # Takes in a list of correlation matrices by cluster group
  # Plots groups as separate facets
  
  # If plotting one matrix
  if(is.matrix(data)) {
    data <- data.frame(item1 = rownames(data), as.data.frame(data))
    p <- data %>% 
      gather(item2, cor, 2:(ncol(.))) %>% 
      mutate(cor = ifelse(item1 > item2, cor, 0),
             label = ifelse(item1 >= item2, round(cor, 2), '')) %>% 
      ggplot(aes(x = item1, y = item2, fill = cor)) + 
      geom_tile() +
      geom_text(aes(label=label)) + 
      scale_fill_gradient2(low=blue, high=orange, mid = 'white') + 
      labs(
        x = '',
        y = '',
        fill = '',
        title = title
      ) + 
      theme_bw() +
      theme(axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = c(.25, .9))
    return(p)
      
  }
  
  # If plotting multiple groups 
  if (is.list(data)) {
    d <- data.frame()
    
    for (group in names(cors)) {
      d <- rbind(d, data.frame(data[[group]], group=group))
    }
    
    p <- d %>% 
      mutate(item1 = rownames(.)) %>% 
      gather(item2, cor, aff:self) %>% 
      mutate(cor = ifelse(item1 < item2, 0, cor),
             alpha = ifelse(item1 < item2, 0, 1),
             alpha = ifelse(cor == 1, 0, alpha),
             cor = ifelse(cor == 1, 0, cor),
             item1 = gsub('\\d', '', item1),
             label = ifelse(item1 <= item2, '', round(cor, 2))) %>% 
      ggplot(aes(x = item1, y = item2, fill = cor)) + 
      geom_tile(aes(alpha = alpha)) + 
      geom_text(aes(label=label), size = 5) + 
      scale_fill_gradient2(low = blue, high = orange, mid = 'white')  + 
      facet_grid(group~.) + 
      labs(
        x = '',
        y = '',
        fill = '',
        title = title
      ) + 
      theme_bw() + 
      theme(axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = NA),
            legend.position = c(.15, .9),
            text = element_text(size = 18)) + 
      guides(alpha = 'none')
    
    return(p)    
      
  }
}
