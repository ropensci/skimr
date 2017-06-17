sk_print_factor <- function(y){
  
  counts <- y %>% dplyr::filter_ (~stat == "count") 
  counts <- paste(paste(counts$level, counts$value, sep = ":"), collapse = " ")
  
  y <- y %>% dplyr::filter_(~stat != "count") %>% 
    dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'value')
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
