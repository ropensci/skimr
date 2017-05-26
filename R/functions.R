#' @include stats.R
NULL


# Default summarizing functions for each type

numeric_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  mean = purrr::partial(mean, na.rm = TRUE),
  sd = purrr::partial(sd, na.rm = TRUE),
  min = purrr::partial(min, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  quantile = purrr::partial(quantile, probs = c(.25, .75), na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  hist = inline_hist
)

factor_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  count = purrr::partial(table, useNA = "always"),
  n_unique = purrr::compose(length, levels)
)

character_funs <- list (
  missing  = n_missing,
  complete = n_complete,
  n = length,
  min = min_char,
  max = max_char,
  empty = n_empty,
  n_unique = purrr::compose(length, n_unique)
)

logical_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  count = purrr::partial(table, useNA = "always"),
  mean = purrr::partial(mean, na.rm = TRUE)
)

integer_funs <- numeric_funs


complex_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length
)


date_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  n_unique = purrr::compose(length, n_unique)
)

.summary_functions_default <- list(
  numeric = numeric_funs,
  integer = integer_funs,
  factor = factor_funs,
  ordered = factor_funs,
  character = character_funs,
  logical = logical_funs,
  complex = complex_funs,
  date = date_funs,
  Date = date_funs
)

.summary_functions <- .summary_functions_default


#' Set or add the summary functions for a particular type of data
#' 
#' @param ... A list of functions, with an argument name that matches a
#'   particular data type.
#' @param append Whether the provided functions should be in addition to the
#'   defaults already in skim.
#' @param reset Whether to revert to the original defaults.
#' @export

skim_with <- function(..., append = TRUE, reset = FALSE) {
  if (reset) {
    .summary_functions <- .summary_functions_default
  } else {
    funs < - list(...)
    all <- purrr::map2(names(funs), funs, set_functions, append)
  }
}

set_functions <- function(type, functions, append) {
  new <- type %in% names(.summary_functions)
  
  if (new) {
    message("Adding new summary functions for type:", type)
  } else if (append) {
    old <- .summary_functions[[type]]
    .summary_functions[[type]] <- c(old, functions)
  } else {
    .summary_functions[[type]] <- functions
  }
}


#' Show summary functions currently used, by column type
#' 
#' @return Nothing. \code{invisible()}
#' @export

show_skimmers <- function() {
  purrr::map(.summary_functions, names)
}


# A method for getting a set of summary functions, by type
#
# @param type The type of summary functions to extract
# @return A list of summary functions
# @keywords internal
# @export

get_funs <- function(type) {
  .summary_functions[[type]]
}
