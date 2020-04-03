#' Create a skimr function list
#'
#' This constructor is used to create a named list of functions. It also you
#' also pass `NULL` to identify a skimming function that you wish to remove.
#' Only functions that return a single value, working with [dplyr::summarize()],
#' can be used within `sfl`.
#'
#' `sfl()` will automatically generate callables and names for a variety of
#' inputs, including functions, formulas and strings. Nonetheless, we recommend
#' providing names when reasonable to get better [skim()] output.
#' @param ... Inherited from dplyr::data_masking() for dplyr version 1 or later 
#'  or dplyr::funs() for older versions of dplyr.
#'  A list of functions
#'   specified by:
#'
#'  - Their name, `"mean"`
#'  - The function itself, `mean`
#'  - A call to the function with `.` as a dummy argument,
#'    `mean(., na.rm = TRUE)`
#'
#'  The following notations are **not** supported, see examples:
#'
#'  - An anonymous function, `function(x) mean(x, na.rm = TRUE)`
#'  - An anonymous function in \pkg{purrr} notation, `~mean(., na.rm = TRUE)`
#' @param skim_type A character scalar. This is used to match locally-provided
#'   skimmers with defaults. See [get_skimmers()] for more detail.
#' @return A `skimr_function_list`, which contains a list of `fun_calls`,
#'   returned by `dplyr::funs()` and a list of skimming functions to drop.
#' @seealso [dplyr::funs()], [skim_with()] and [get_skimmers()].
#' @examples
#' # sfl's can take a variety of input formats and will generate names
#' # if not provided.
#' sfl(mad, "var", ~ length(.)^2)
#'
#' # But these can generate unpredictable names in your output.
#' # Better to set your own names.
#' sfl(mad = mad, variance = "var", length_sq = ~ length(.)^2)
#'
#' # sfl's can remove individual skimmers from defaults by passing NULL.
#' sfl(hist = NULL)
#'
#' # When working interactively, you don't need to set a type.
#' # But you should when defining new defaults with `get_skimmers()`.
#' get_skimmers.my_new_class <- function(column) {
#'   sfl(n_missing, skim_type = "my_new_class")
#' }
#' @export
sfl <- function(..., skim_type = "") {
  stopifnot(length(skim_type) == 1, is.character(skim_type))
  funs <- build_sfl_names(...)
  structure(
    list(funs = funs, skim_type = skim_type),
    class = "skimr_function_list"
  )
}

build_sfl_names <- function(...) {
  labels <- rlang::quos_auto_name(rlang::enquos(...))
  rlang::set_names(rlang::list2(...), names(labels))
}
