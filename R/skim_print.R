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
  types_custom <- names(print_handling)
  
  if ("grouped_df" %in% class(x)){
    group_by_vars <- as.character(attr(x, "vars"))
    group_by_vars_title <- paste(group_by_vars, collapse = ", ")
    x <- tidyr::unite_(x, "var", c("var", group_by_vars), sep = "__")
    attr(x, "group_by_vars") <- group_by_vars_title
    
  }
  
  for (i in 1:length(types)){
    one_type <- x %>% dplyr::filter_(~type == types[i])
    if (nrow(one_type) > 0){
      if (types[i] %in% types_custom){
        p <- print_handling[[types[i]]](one_type)
      } else {
        p <- print_handling[["default"]](one_type)
      }

      if (!is.null(group_by_vars_title)){
        p <- tidyr::separate(p, var, into = c("var", group_by_vars),  sep = "__")

      }
      
      return_list[[types[i]]] <- p
      if (!is.null(attr(x, "group_by_vars"))){
        attr(return_list[[types[i]]], "subtibble_title") <- paste(stringr::str_to_title(types[i]), "Variables", 
                                                                  "grouped by", group_by_vars_title, "\n")
      } else {
      attr(return_list[[types[i]]], "subtibble_title") <- paste(stringr::str_to_title(types[i]), "Variables\n")
    }
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




