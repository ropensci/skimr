#' Use dplyr verb filter on skim_df objects.
#' 
#' @seealso [`dplyr::filter()`]
#' @param  .data A skim object
#' @param  ... Logical predicates defined in terms of the variables in .data.
#' @param  .preserve When FALSE (the default) the grouping structuree is recalculated
#'                   based on the resulting data.
#' @return  skim_df object coerced to a data frame.
#' @export
filter.skim_df <-function (.data, ..., .preserve = FALSE) {
  .data <- as.data.frame(.data)
  .data <- dplyr::filter(.data, ...)
  class(.data)  <- c("tbl_df", "tbl", "data.frame")
  .data
}

#' Use dplyr verb select on skim_df objects.
#' 
#' @seealso [`dplyr::select()`]
#' @param  .data A skim object
#' @param  ... One or more unquoted expressions separated by commas.
#' @return  skim_df object coerced to a data frame.
#' @export
select.skim_df <-function (.data, ...) {
  .data <- as.data.frame(.data)
  .data <- dplyr::select(.data, ...)
  class(.data)  <- c("tbl_df", "tbl", "data.frame")
  .data
}

#' Use dplyr verb mutate on skim_df objects.
#' 
#' @seealso [`dplyr::mutate()`]
#' @param  .data A skim object
#' @param  ... Name-value pairs of expressions. The name of each argument will 
#'             be the name of a new variable, and the value will be its 
#'             corresponding value.
#' @return  skim_df object coerced to a data frame.
#' @export
mutate.skim_df <-function (.data, ...) {
  .data <- as.data.frame(.data)
  .data <- dplyr::mutate(.data, ...)
  class(.data)  <- c("tbl_df", "tbl", "data.frame")
  .data
}

#' Use dplyr verb arrange on skim_df objects.
#' 
#' @seealso [`dplyr::arrange()`]
#' @param  .data A skim object
#' @param  ... Comma separated list of unquoted variable names, or expressions 
#'             involving variable names.
#' @param  .by_group	If TRUE, will sort first by grouping variable. 
#'                    Applies to grouped data frames only.
#' @return  skim_df object coerced to a data frame.
#' @export
arrange.skim_df <-function (.data, ..., .by_group = FALSE) {
  .data <- as.data.frame(.data)
  .data <- dplyr::arrange(.data, ...)
  class(.data)  <- c("tbl_df", "tbl", "data.frame")
  .data
}

#' Use dplyr verb slice on skim_df objects.
#' 
#' @seealso [`dplyr::slice()`]
#' @param  .data A skim object
#' @param  ... Integer row calues. Provide either positive values to keep or
#'             negative values to drop.
#' @param  .preserve  when FALSE (the default), the grouping structure is 
#'                    recalculated based on the resulting data, otherwise 
#'                    it is kept as is.
#' @return  skim_df object coerced to a data frame.
#' @export
slice.skim_df <-function (.data, ..., .preserve = FALSE) {
  .data <- as.data.frame(.data)
  .data <- dplyr::slice(.data, ...)
  class(.data)  <- c("tbl_df", "tbl", "data.frame")
  .data
}
