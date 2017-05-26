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

#' Generate inline histgram
#' @export
inline_hist <- function(x) {
  x <- x[!is.na(x)]
  hist_dt <- table(cut(x, 10))
  hist_dt <- hist_dt / max(hist_dt)
  out <- 0
  names(out) <- colformat::spark_bar(hist_dt)
  return(out)
}
