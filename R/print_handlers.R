sk_print_factor <- function(one_type){

  counts <- one_type %>% dplyr::filter_ (~stat == "count") %>% dplyr::group_by_(~var) 
  counts$level <- ifelse(is.na(counts$level), "NA", counts$level)
  united <- counts %>%
            tidyr::unite_( "col_sum", c("level", "value"), sep = ":") %>%
            dplyr::summarise_( count_sum= ~paste(col_sum, collapse = " "))

  one_type$stat <- ifelse(one_type$level == ".all" | one_type$stat == "count" , one_type$stat,  paste(one_type$stat, one_type$level))
  one_type <- one_type %>% dplyr::filter_(~stat != "count") %>% 
       dplyr::select_(.dots = c('var', 'stat', 'formatted_value')) %>% 
       tidyr::spread_( key_col = 'stat', value_col = 'formatted_value')
  counts<-united$count_sum

  # Limit the length of counts formatted value.
  counts <-ifelse (nchar(counts) > 45, paste0(substr(counts, 1,45), "..."), counts)
   
  one_type <- tibble::add_column(one_type, counts)

  one_type
}

sk_print_default<-function(y){
  y$stat <- ifelse(y$level == ".all", y$stat,  paste(y$stat, y$level))
  stat_order<-unique(y$stat)
  y <- y %>% dplyr::select_(.dots = c('var', 'stat', 'formatted_value')) %>% 
    tidyr::spread_( key_col = 'stat', value_col = 'formatted_value')
  y <- y[c("var", stat_order)]
  y
}

# Define the print functions for different classes.
print_handling <- list(
  factor = sk_print_factor,
  ordered = sk_print_factor,
  default = sk_print_default
)