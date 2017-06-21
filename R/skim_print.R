#' @include print_handlers.R

#' @title skim_print
#' 
#' @description Manages print for skim objects.
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
  
  print(subtibble)
}




