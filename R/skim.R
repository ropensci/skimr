globalVariables(".")

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
  combined <- dplyr::bind_rows(rows, .id = "variable")
  structure(combined, class = c("skim_df", class(combined)),
            data_rows = nrow(.data), data_cols = ncol(.data))
}

#' @export

skim.grouped_df <- function(.data) {
  skimmed <- dplyr::do(.data, skim(.))
  
  # Drop the grouping variable
  groups <- dplyr::groups(skimmed)
  to_drop <- quote(!(variable %in% groups))
  skimmed <- dplyr::filter(skimmed, !!to_drop)
  structure(skimmed, class = c("skim_df", class(skimmed)),
            data_rows = nrow(.data), data_cols = ncol(.data))
}

#' Print useful summary statistic from a data frame without modification
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return The input data frame.
#' @export

skim_tee <- function(.data) {
  print(skim(.data))
  invisible(.data)
}

#' Print skim result and return a wide data frame of summary statistics
#' 
#' Prints a skimmed data frame (created by skim()) and returns a wide data frame with one
#'  row per variable and NA for statistics not calculated for a given type.
#' 
#' @param x A \code{dataframe}.
#' @param ... Further arguments passed to or from other methods.
#' @return A wide data frame.
#' @examples 
#'   skim_wide(iris)
#'   iris %>% skim_wide()
#'   iris %>% skim_wide() %>% dplyr::filter(type == "factor") %>% 
#'            dplyr::select(top_counts)
#' @export

skim_wide <- function(x, ...) {
  x <- skim(x, ...)
  grps <- dplyr::groups(x)
  grouped <- dplyr::group_by(x, !!!quote(type))
  x <- dplyr::do(grouped, skim_render(., grps, quiet_impl, ...))
  dplyr::ungroup(x)
}
