globalVariables(".")

#' Get useful summary statistic from a data frame
#' 
#' \code{skim} handles data of all types, dispatching a different set of
#' summary functions based on the types of columns in the data frame.
#' It is an intentionally simple function. See \code{\link{skim_with}} and
#' \code{\link{skim_format}} for how \code{skim} can be customized.
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @param ...  Additional options, normally used to list individual unquoted column names.
#' @return A \code{skim_df} object, which can be treated like a
#'  tbl in most instances.
#' @examples
#' skim(iris)
#' 
#' # Skim also works groupwise
#' dplyr::group_by(iris) %>% skim()
#' @export

skim <- function(.data, ...) {
  UseMethod("skim")
}

#'@export

skim.data.frame <- function(.data, ... ) {
  .vars <- rlang::quos(...)
  if (length(.vars) == 0)  selected <- tidyselect::everything(.data)
  else  selected <- tidyselect::vars_select(names(.data), !!! .vars) 

  rows <- purrr::map(.data[selected], skim_v)
  combined <- dplyr::bind_rows(rows, .id = "variable")
  structure(combined, class = c("skim_df", class(combined)),
            data_rows = nrow(.data), data_cols = length(selected))
}

#' @export

skim.grouped_df <- function(.data, ...) {
  skimmed <- dplyr::do(.data, skim(., ...))
  
  # Drop the grouping variable
  groups <- dplyr::groups(skimmed)
  to_drop <- quote(!(variable %in% groups))
  skimmed <- dplyr::filter(skimmed, !!to_drop)
  structure(skimmed, class = c("skim_df", class(skimmed)),
            data_rows = nrow(.data), data_cols = ncol(.data))
}

#' Print useful summary statistic from a data frame returning the data frame
#'  without modification
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @param ...  Additional options, normally used to list individual unquoted column names.
#' @return The input data frame.
#' @export

skim_tee <- function(.data, ...) {
  print(skim(.data))
  invisible(.data)
}
