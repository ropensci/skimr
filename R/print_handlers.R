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

sk_print_default<-function(y){

  order<-unique(y$stat)
  y <- y %>% dplyr::select_(.dots = c('var', 'stat', 'formatted_value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'formatted_value')

  y[c("var", order)]
}

# Define the print functions for different classes.
print_handling <- list(
  
  numeric = sk_print_default,
  
  double =  sk_print_default,
  
  integer = sk_print_default,
  
  factor = sk_print_factor,
  
  ordered = sk_print_factor,
  
  character = sk_print_default,
  
  default = sk_print_default
  
)