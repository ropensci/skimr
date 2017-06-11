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
    one_type <- x %>% filter(type == types[i])
    if (nrow(one_type) > 0){
      if (types[i] %in% types_custom){
        p <- print_handling[[types[i]]](one_type)
      } else {
        p <- print_handling[["default"]](one_type)
      }
      
      return_list[[types[i]]] <- p
      attr(return_list[[types[i]]], "subtibble_title") <- paste(types[i], "Variables\n")
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


# Define the print functions for different classes.
print_handling <- list(

  numeric = numeric <- function(y){
  
    order<-unique(y$stat)
    type <- "Numeric"
    histograms <- y %>% dplyr::filter(stat == "hist")
    y <- y %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
    
    y$hist <- histograms$level
    
    y[c("var", order)]
    # return_list[[type]] <- y
    # attr(return_list[[type]], "subtibble_title") <- paste(type, "Variables\n")
    # return_list
  },

  double = double <- numeric,
  
  integer = integer <- numeric,

  factor = factor <- function(y){
  
      type = "Factor"
      counts <- y %>% dplyr::filter (stat == "count") 
      counts <- paste(paste(counts$level, counts$value, sep = ":"), collapse = " ")
      
      y <- y %>% dplyr::filter(stat != "count") %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
      y <- tibble::add_column(y, counts)
      y
  },

  ordered = ordered <- factor,

  character = character<-function(y){
      type = "Character"
      order<-unique(y$stat)
      y <- y %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
      y[c("var", order)]
  },

  default = default<-function(y){
      order<-unique(y$stat)
      z <- y %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
      z[c("var", order)]
  }
)