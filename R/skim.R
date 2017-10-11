#' Get useful summary statistic from a data frame
#' 
#' \code{skim} handles data of all types, dispatching a different set of
#' summary functions based on the types of columns in the data frame.
#' It is an intentionally simple function. See \code{\link{skim_with}} and
#' \code{\link{skim_format}} for how \code{skim} can be customized.
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'  tbl in most instances.
#' @examples
#' skim(iris)
#' 
#' # Skim also works groupwise
#' dplyr::group_by(iris) %>% skim()
#' @export

skim <- function(.data) {
  UseMethod("skim")
}

#'@export

skim.data.frame <- function(.data) {
  rows <- purrr::map(.data, skim_v)
  combined <- dplyr::bind_rows(rows, .id = "var")
  attr(combined, "data_rows") <- nrow(.data)
  attr(combined, "data_cols") <- ncol(.data)
  structure(combined, class = c("skim_df", class(combined)))
}

#' @export

skim.grouped_df <- function(.data) {
  skimmed <- dplyr::do(.data, skim(.))
  structure(skimmed, class = c("skim_df", class(skimmed)))
}

#' Get useful summary statistic from a data frame, print it, and return it
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'  tbl in most instances.
#' @export

skim_tee <- function(.data) {
  print(skim(.data))
  invisible(.data)
}
