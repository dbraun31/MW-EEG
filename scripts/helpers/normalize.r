custom_scale <- function(response, response_m, response_sd) {
  # Check to ensure there's a non zero sd
  scaled <- (response - response_m) / response_sd
  out <- ifelse(response_sd != 0 & !is.na(response_sd), scaled, response_m)
  return(out)
}

normalize_subject_item <- function(d) {
  # Normalize within subject and item
  gather_cols <- colnames(d)
  gather_cols <- gather_cols[!gather_cols %in% c('subject', 'item', 'conf')]
  d <- d %>% 
    mutate(id = 1:(nrow(d))) %>% 
    gather(item, response, gather_cols[1]:gather_cols[length(gather_cols)]) %>% 
    group_by(subject, item) %>% 
    mutate(response_m = mean(response), response_sd = sd(response)) %>% 
    ungroup() %>% 
    mutate(response_sc = custom_scale(response, response_m, response_sd)) %>% 
    select(-response, -response_m, -response_sd) %>% 
    rename(response = response_sc) %>% 
    spread(item, response) %>% 
    select(-id) 
  return(d)
}
