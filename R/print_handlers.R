sk_print_factor <- function(y){
  
  counts <- y %>% dplyr::filter_ (~stat == "count") %>% dplyr::group_by(var) 
  counts$level <- ifelse(is.na(counts$level), "NA", counts$level)
  counts <- dplyr::summarise(counts, count_sum = paste(paste(~level, ~value, sep = ":"), collapse = " "))

  y <- y %>% dplyr::filter_(~stat != "count") %>% 
    dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'value')
  counts<-counts$count_sum
  y <- tibble::add_column(y, counts)
  y
}

sk_print_numeric <- function(y){
  
  order<-unique(y$stat)
  histograms <- y %>% dplyr::filter_(~stat == "hist")
  y <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'value')
  y$hist <- histograms$level
  
  y[c("var", order)]
}

sk_print_character<-function(y){
  order<-unique(y$stat)
  y <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'value')
  
  y[c("var", order)]
}

sk_print_default<-function(y){
  order<-unique(y$stat)
  z <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>%
    tidyr::spread_( key_col = 'stat', value_col = 'value')
  
  z[c("var", order)]
}
