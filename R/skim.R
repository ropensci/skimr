#' Get useful summary statistic from a data frame
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'   tbl in most instances.
#' @export

skim <- function(.data) {
  UseMethod("skim")
}

#' Get useful summary statistic from a data frame, print it, and return it
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'   tbl in most instances.
#' @export
skim_tee <- function(.data) {
  t <- skim(.data)
  print(t)
  invisible(t)
}

#'@export

skim.data.frame <- function(.data) {
  rows <- purrr::map(.data, skim_v)
  combined <- dplyr::bind_rows(rows, .id = "var")
  return(structure(combined, class = c("skim_df", class(combined))))
}
