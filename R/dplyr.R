#' Mutate a skim_df
#'
#' [dplyr::mutate()] currently drops attributes, but we need to keep them around
#' for other skim behaviors. Otherwise the behavior is exactly the same. For
#' more information, see https://github.com/tidyverse/dplyr/issues/3429.
#'
#' @param .data A `skim_df`, which behaves like a `tbl.`
#' @param ... Name-value pairs of expressions, each with length 1 or the same
#'   length as the number of rows in the group (if using [group_by()] or in the
#'   entire input (if not using groups). The name of each argument will be the
#'   name of a new variable, and the value will be its corresponding value.  Use
#'   `NULL` value in `mutate` to drop a variable.  New variables overwrite
#'   existing variables of the same name.
#'
#'   The arguments in `...` are automatically [quoted](rlang::quo) and
#'   [evaluated](rlang::eval_tidy) in the context of the data frame. They
#'   support [unquoting](rlang::quasiquotation) and splicing. See
#'   `vignette("programming", package = "dplyr")` for an introduction to these
#'   concepts.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. In many ways, the object behaves like a [tibble::tibble()].
#' @seealso [dplyr::mutate()] for the function's expected behavior.
#' @importFrom dplyr mutate
#' @export
mutate.skim_df <- function(.data, ...) {
  mutated <- NextMethod("mutate")
  if (could_be_skim_df(mutated)) {
    reassign_skim_attrs(mutated, .data)
  } else {
    strip_skim_attrs(mutated)
  }
}
