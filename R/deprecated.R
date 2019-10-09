#' Deprecated functions from skimr v1
#'
#' Skimr used to offer functions that combined skimming with a secondary effect,
#' like reshaping the data, building a list or printing the results. Some of
#' these behaviors are no longer necessary. [skim()] always returns a wide
#' data frame. Others have been replaced by functions that do a single thing.
#' [partition()] creates a list-like object from a skimmed data frame.
#'
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Columns to select for skimming. When none are provided, the
#'   default is to skim all columns.
#' @return Either A `skim_df` or a `skim_list` object.
#' @name deprecated-v1
NULL

#' @describeIn deprecated-v1 [skim()] always produces a wide data frame.
#' @export
skim_to_wide <- function(.data, ...) {
  .Deprecated("skim()")
  skim(.data, ...)
}

#' @describeIn deprecated-v1 [partition()] creates a list.
#' @export
skim_to_list <- function(.data, ...) {
  .Deprecated("partition(skim())")
  skim(.data, ...) %>% partition()
}

#' @describeIn deprecated-v1 [print()] and [skim_with()] set options.
#' @export
skim_format <- function(...) {
  .Deprecated(
    "print()",
    msg = paste(
      "Formatting options are now in print() or as function",
      "arguments in skim_with()."
    )
  )
}
