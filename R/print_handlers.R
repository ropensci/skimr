sk_print_factor <- function(y){
  
  counts <- y %>% dplyr::filter_ (~stat == "count") %>% dplyr::group_by_(~var) 
  counts$level <- ifelse(is.na(counts$level), "NA", counts$level)
  united <- counts %>%
    tidyr::unite_( "col_sum", c("level", "value"), sep = ":") %>%
    dplyr::summarise_( count_sum= ~paste(col_sum, collapse = " "))
  
  y <- y %>% dplyr::filter_(~stat != "count") %>% 
    dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'value')
  counts<-united$count_sum
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
#  print(as.data.frame(y))
#  y$stat <- ifelse(y$level == ".all", y$stat, )
  order<-unique(y$stat)
  z <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>%
    tidyr::spread_( key_col = 'stat', value_col = 'value')
  
  z[c("var", order)]
}

# Define the print functions for different classes.
print_handling <- list(
  
  sk_print_numeric = sk_print_numeric,
  
  sk_print_double =  sk_print_numeric,
  
  sk_print_integer = sk_print_numeric,
  
  sk_print_factor = sk_print_factor,
  
  sk_print_ordered = sk_print_factor,
  
  sk_print_character = sk_print_character,
  
  sk_print_default = sk_print_default
  
)