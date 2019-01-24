#' Separate a big `skim_df` into smaller data frames, by type.
#'
#' The data frames produced by [skim()] are wide and sparse, filled with
#' columns that are mostly `NA`. For that reason, it can be convenient to
#' work with "by type" subsets of the original data frame. These smaller
#' subsets have their `NA` columns removed.
#'
#' `partition()` creates a list of smaller `skim_df` data frames. Each entry
#' in the list is a data type from the original `skim_df`. The inverse of
#' `partition()` is `bind()`, which takes the list and produces the original
#' `skim_df`. While `partition()` keeps all of the subtables as list entries,
#' `yank()` gives you a single subtable for a data type.
#'
#' @param data A `skim_df`.
#' @param type A character scalar. The subtable to extract from a `skim_df`.
#' @return A `skim_list` of `skim_df`'s, by type.
#' @examples
#' # Create a wide skimmed data frame (a skim_df)
#' skimmed <- skim(iris)
#'
#' # Separate into a list of subtables by type
#' separate <- partition(skimmed)
#'
#' # Put back together
#' identical(bind(separate), skimmed)
#' # > TRUE
#'
#' # Alternatively, get the subtable of a particular type
#' yank(skimmed, "factor")
#' @export
partition <- function(data) {
  as_list <- split(data, data$type)
  skimmers <- attr(data, "skimmers_used")
  groups <- attr(data, "groups")
  reduced <- purrr::imap(as_list, simplify_skimdf, skimmers, groups)
  structure(reduced,
    class = "skim_list",
    data_rows = attr(data, "data_rows"),
    data_cols = attr(data, "data_cols"),
    df_name = attr(data, "df_name"),
    groups = groups,
    skimmers_used = skimmers
  )
}

simplify_skimdf <- function(data, type, skimmers, groups) {
  keep <- c("variable", groups, skimmers[[type]])
  cols_in_data <- names(data)
  out <- dplyr::select(data, !!!dplyr::intersect(keep, cols_in_data))
  structure(out,
    class = c("one_skim_df", "tbl_df", "tbl", "data.frame"),
    type = type
  )
}

#' @describeIn partition The inverse of a `partition()`. Rebuild the original
#'   `skim_df`.
#' @export
bind <- function(data) {
  combined <- dplyr::bind_rows(!!!data, .id = "type")
  # The variable column should always be first
  out <- dplyr::select(combined, !!rlang::sym("variable"), dplyr::everything())
  structure(out,
    class = c("skim_df", "tbl_df", "tbl", "data.frame"),
    data_rows = attr(data, "data_rows"),
    data_cols = attr(data, "data_cols"),
    df_name = attr(data, "df_name"),
    skimmers_used = attr(data, "skimmers_used")
  )
}

#' @describeIn partition Extract a subtable from a `skim_df` with a particular
#'   type.
#' @export
yank <- function(data, type) {
  partition(data)[[type]]
}

#' Only show a subset of summary statistics after skimming
#'
#' This function is a variant of [dplyr::select()] designed to work with
#' `skim_df` objects. When using `focus()`, `skimr` metadata columns are kept,
#' and `skimr` print methods are still utilized. Otherwise, the signature and
#' behavior is identical to [dplyr::select()].
#'
#' @param .data A `skim_df` object.
#' @inheritParams dplyr::select
#' @examples
#' # Compare
#' iris %>%
#'   skim() %>%
#'   dplyr::select(missing)
#'
#' iris %>%
#'   skim() %>%
#'   focus(missing)
#'
#' # This is equivalent to
#' iris %>%
#'   skim() %>%
#'   dplyr::select(variable, type, missing)
#' @export
focus <- function(.data, ...) {
  stopifnot(inherits(.data, "skim_df"))
  dplyr::select(.data, "variable", "type", ...)
}

#' Skim to a wide data frame
#' Deprecated
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Columns to select for skimming. When none are provided, the
#'   default is to skim all columns.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. The result is usually a data frame or tibble.
#' @export
skim_to_wide<- function(.data, ...){
  .Deprecated("skim()")
  skim(.data, ...)
}

#' Skim results returned as a tidy long data frame with four columns:
#' variable, type, stat and formatted.
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Columns to select for skimming. When none are provided, the
#'   default is to skim all columns.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. The result is usually a data frame or tibble.
#' @examples
#' to_long(iris)
#' @export
to_long <- function( .data, ...){
  skimmed <- skim(.data, ...)
  tidyr::gather(skimmed, key="stat", value="formatted",  na.rm = TRUE, 
                -!!rlang::sym("type"), -!!rlang::sym("variable")) 
}

#' Skim to a list of data frames by type
#' Deprecated
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Columns to select for skimming. When none are provided, the
#'   default is to skim all columns.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. The result is usually a data frame or tibble.
#' @export
skim_to_list<- function(.data, ...){
  .Deprecated("partition(skim())")
  skim(.data, ...) %>% partition()
}
