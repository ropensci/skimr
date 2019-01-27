#' Create a skimr function list
#'
#' This is an extension of [dplyr::funs()]. It is used to create a named list
#' of functions. It also you also pass `NULL` to identify a skimming function
#' that you wish to remove. Only functions that return a single value, working
#' with [dplyr::summarize()], can be used within `sfl`.
#'
#' @inheritParams dplyr::funs
#' @param .type A character scalar. This is used to match locally-provided
#'   skimmers with defaults. See [get_skimmers()] for more detail.
#' @return A `skimr_function_list`, which contains a list of `fun_calls`,
#'   returned by [dplyr::funs()] and a list of skimming functions to drop.
#' @seealso [dplyr::funs()], [skim_with()] and [get_skimmers()].
#' @export
sfl <- function(..., .args = list(), .type = "") {
  skimmer_list <- rlang::enquos(...)
  if (length(skimmer_list) < 1) {
    stop("Please provide one or more named argument")
  }

  stopifnot(length(.type) == 1, is.character(.type))

  dropable <- purrr::map_lgl(skimmer_list, rlang::quo_is_null)
  keep <- skimmer_list[!dropable]
  drop <- skimmer_list[dropable]
  out <- list(
    keep = dplyr::funs(!!!keep, .args = .args), drop = names(drop),
    type = .type
  )

  structure(out, class = "skimr_function_list")
}
