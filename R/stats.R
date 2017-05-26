# Summary statistic functions

#' Calculate missing values
#' 
#' @param x A vector
#' @return The sum of NULL and NA values
#' @export

n_missing <- function(x) {
  sum(is.na(x) | is.null(x))
}


#' Calculate complete values
#' 
#' Complete values are not missing
#' @param x A vector
#' @return The sum of non-NULL and non-NA values
#' @export

complete <- function(x) {
  length(x) - n_missing(x)
}
