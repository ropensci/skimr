# Convenience functions

#' Filter a skim_df for a single type
#' Takes a skim_df and returns a just those rows for a single data type
#' (e.g "numeric", "factor").
#' @param x a skim_df object
#' @param type_selected name of the data type to select (e.g. "numeric")
#' @examples 
#'   skim(iris) %>% skim_filter_type("numeric")
#' @export
skim_filter_type <- function(x, type_selected){
  if (!(type_selected %in% unique(x$type))){
    return(message("No variables of type ", type_selected," are present in the skim object"))
  }
  x <- x %>% dplyr::filter((!!quote((type == type_selected))))
  class(x) <- c( "skim_df",class(x))
  x
}

#' Create wide dataframe of formatted values in a skim_df object
#' 
#' \code{skim_wide_formatted} converts a \code{skim_df} object into a wide data frame
#' with one row for each variable in the original data frame and one column for each unique 
#' statistic containing the formatted values.
#' Variables with levels (e.g. factors) are handled by appending '_1', '_2' and so on.
#' This function provides users with some additional flexibility for displaying or processing 
#' the results of skimming. 
#' Formatted values are always characters.
#' @param x a skim_df object
#' @examples 
#' skim(iris) %>% skim_wide_formatted() 
#' skim(iris) %>% skim_filter_type("numeric") %>% skim_wide_formatted() 
#' @export
skim_wide_formatted <- function(x){
  x <- rename_level_stats(x)
  x %>% dplyr::select( !!quote(variable), !!quote(formatted), !!quote(stat) ) %>%
    tidyr::spread(key = !!quote(stat), value = !!quote(formatted))
  
}

#' Create wide dataframe of values in a skim_df object
#' 
#' \code{skim_wide_value} converts a \code{skim_df} object into a wide data frame
#' with one row for each variable in the original data frame and one column for each unique 
#' statistic containing the formatted values.
#' Variables with levels (e.g. factors) are handled by appending '_1', '_2' and so on.
#' This function provides users with some additional flexibility for displaying or processing 
#' the results of skimming. 
#' Values are always numeric.
#' 
#' @param x a skim_df object
#' @examples 
#' skim(iris) %>% skim_wide_value() 
#' skim(iris) %>% skim_filter_type("numeric") %>% skim_wide_value()
#' @export
skim_wide_value <- function(x){
  x <- rename_level_stats(x)
  x %>% dplyr::select( !!quote(variable), !!quote(value), !!quote(stat) ) %>% 
       tidyr::spread(key = !!quote(stat), value = !!quote(value))
  
}

rename_level_stats <- function(x){
  x <- x  %>%  
    dplyr::group_by(!!quote(variable), !!quote(stat))  %>% 
    dplyr::mutate(new_stat = !!quote(paste0(stat, "_", row_number()))) %>% 
    dplyr::ungroup()
  x$stat <- ifelse(x$level != ".all" | is.na(x$level), x$new_stat, x$stat)
  x
}
