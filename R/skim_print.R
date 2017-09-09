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

  types <- unique(x$type)
  types_custom <- names(print_handling)
  return_list <- list()

  for (i in 1:length(types)){

    return_list[[types[i]]] <- print_type(x, types[i])
    attr(return_list[[types[i]]], "subtibble_title") <- paste(stringr::str_to_title(types[i]), "Variables\n")

    return_list
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


#' @title skim_print
#'
#' @description Manages print for grouped skim objects.
#'
#' @param x A \code{skim_grouped_df} object
#' @importFrom dplyr select mutate filter data_frame left_join
#' @importFrom tidyr gather spread
#' @return A \code{skim_df} object
#' @export
skim_grouped_print <- function(x){

  # Manage groups
  group_by_vars <- as.character(attr(x, "vars"))
  group_by_vars_title <- paste(group_by_vars, collapse = ", ")

  # Give each combination of variables and groups a unique name.
  x <- tidyr::unite_(x, "var", c("var", group_by_vars), sep = "__")
  attr(x, "group_by_vars") <- group_by_vars_title

  types <- unique(x$type)
  types_custom <- names(print_handling)
  return_list <- list()
 
  for (i in 1:length(types)){

    return_list[[types[i]]] <- print_type(x, types[i])

    # Separate the unique group-variable names back into distinct columns 
    return_list[[types[i]]] <- tidyr::separate_(return_list[[types[i]]], "var", into = c("var", group_by_vars),  sep = "__")
    attr(return_list[[types[i]]], "subtibble_title") <- paste(stringr::str_to_title(types[i]), "Variables",
                                                              "grouped by", group_by_vars_title, "\n")
    
  }

  return(return_list)
}

#' @export
print.skim_grouped_df <- function(x, ...) {

  lapply(skim_grouped_print(x),print_results)
}

print_type <- function(x, type_name){
    one_type <- x %>% dplyr::filter(type == type_name)
    types_custom <- names(print_handling)

    if (nrow(one_type) > 0){
      if (type_name %in% types_custom){
        p <- print_handling[[type_name]](one_type)
      } else {
        p <- print_handling[["default"]](one_type)
      }
 
      p
    }
}
