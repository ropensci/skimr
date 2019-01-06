#' Use dplyr verb filter on skim_df objects.
#' 
#' @seealso [`dplyr::filter()`]
#' @inheritParams dplyr::filter
#' @return  skim_df object coerced to a data frame.
#' @export
filter.skim_df <-function (.data, ..., .preserve = FALSE) {
  .data <- as.data.frame(.data)
  .data <- dplyr::filter(.data, ...)
  class(.data)  <- c("tbl_df", "tbl", "data.frame")
  .data
}
