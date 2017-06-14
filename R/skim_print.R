#' Manages print for skim objects.
#' 
#' Currently, numeric, factor and character data are handled.
#' 
#' @param x A \code{skim_df} object
#' @importFrom dplyr select mutate filter data_frame left_join
#' @importFrom tidyr gather spread
#' @return A \code{skim_df} object
#' @export
skim_print <- function(x){
 
  x$stat <- ifelse(x$level == ".all" | x$stat == "hist" | x$stat == "count", x$stat,  paste(x$stat, x$level))
  return_list <- list()
  types <- unique(x$type)
  types_custom <- c("numeric", "double", "integer", "factor", "character", "ordered")

  for (i in 1:length(types)){
    one_type <- x %>% dplyr::filter_(~type == types[i])
    if (nrow(one_type) > 0){
      if (types[i] %in% types_custom){
        p <- print_handling[[paste0("sk_print_",types[i])]](one_type)
      } else {
        p <- print_handling[["sk_print_default"]](one_type)
      }
      
      return_list[[types[i]]] <- p
      attr(return_list[[types[i]]], "subtibble_title") <- paste(stringr::str_to_title(types[i]), "Variables\n")
      return_list
    }
  }  
  
  return(return_list)   
}

#' @export
print.skim_df <- function(x, ...) {
  lapply(skim_print(x),print_results)
}

print_results<-function(subtibble){
  cat(attr(subtibble, "subtibble_title"))
  
  print(lucid::lucid(subtibble))
}

# nocov start
# Define the print functions for different classes.
print_handling <- list(

  sk_print_numeric = sk_print_numeric <- function(y){
  
    order<-unique(y$stat)
    histograms <- y %>% dplyr::filter_(~stat == "hist")
    y <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
            tidyr::spread_( key_col = 'stat', value_col = 'value')
    y$hist <- histograms$level

    y[c("var", order)]
  },

  sk_print_double = sk_print_double <- sk_print_numeric,
  
  sk_print_integer = sk_print_integer <- sk_print_numeric,

  sk_print_factor = sk_print_factor <- function(y){
  
      counts <- y %>% dplyr::filter_ (~stat == "count") 
      counts <- paste(paste(counts$level, counts$value, sep = ":"), collapse = " ")
      
      y <- y %>% dplyr::filter_(~stat != "count") %>% 
            dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
            tidyr::spread_( key_col = 'stat', value_col = 'value')
      y <- tibble::add_column(y, counts)
      y
  },

  sk_print_ordered = sk_print_ordered <- sk_print_factor,

  sk_print_character = sk_print_character<-function(y){
      order<-unique(y$stat)
      y <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>% 
        tidyr::spread_( key_col = 'stat', value_col = 'value')

      y[c("var", order)]
  },

  sk_print_default = sk_print_default<-function(y){
      order<-unique(y$stat)
      z <- y %>% dplyr::select_(.dots = c('var', 'stat', 'value')) %>%
            tidyr::spread_( key_col = 'stat', value_col = 'value')

      z[c("var", order)]
  }

)
# nocov end
