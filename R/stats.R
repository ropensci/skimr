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

#' Generate inline histgram for numeric variables
#' 
#' @param x A vector
#' @return A character string of histogram.
#' @export
inline_hist <- function(x) {
  x <- x[!is.na(x)]
  hist_dt <- table(cut(x, 10))
  hist_dt <- hist_dt / max(hist_dt)
  out <- 0
  names(out) <- colformat::spark_bar(hist_dt)
  return(out)
}

#' Calculate the number of blank values in a character vector
#' 
#' A "blank" is equal to "".
#' @param x A vector
#' @return The number of values in the vector equal to ""
#' @export

n_empty <- function(x) {
  sum(x == "")
}
