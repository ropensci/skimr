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
  
  if (length(y) > 1 ){
    y[2:ncol(y)] <-lapply(y[2:ncol(y)], number_align)
  }

  y
}

number_align <- function(x){
  number_test <- sum(grepl("^[-]{0,1}[0-9]+(\\.[0-9]+)?$", trimws(x)))
  if (number_test < length(x)){
    return(x)
  }
 
  num_split <- stringr::str_split(x, "\\.", simplify = TRUE)
  
  if (ncol(num_split) == 1 ){
 
    return(x)
  }

  max_whole_digits <- max(nchar(num_split[,1]))
  max_decimal_digits <- max(nchar(num_split[,2]))
  num_split[,1] <- stringr::str_pad(num_split[,1], max_whole_digits, side = "left")
  
  num_split_2 <- num_split[,2]
  num_split[,2] <- stringr::str_pad(num_split[,2], max_decimal_digits, side = "right")
      
  x <- ifelse(num_split_2 != "", 
              paste0(num_split[,1], ".", num_split[,2]), 
              paste0(num_split[,1], " ", num_split[,2])
              )
  x
}



# Define the print functions for different classes.
print_handling <- list(
  factor = sk_print_factor,
  ordered = sk_print_factor,
  default = sk_print_default
)