#' @include stats.R
NULL


# Default summarizing functions for each type -----------------------------

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

ts_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  start = ts_start,
  end = ts_end,
  frequency = frequency,
  deltat = deltat,
  mean = purrr::partial(mean, na.rm = TRUE),
  sd = purrr::partial(sd, na.rm = TRUE),
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE)
)

posixct_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  n_unique = purrr::compose(length, n_unique)  
)

list_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  min_length = list_lengths_min,
  median_length = list_lengths_median,
  max_length = list_lengths_max
)

#listv_funs <- list_funs

.default <- list(
  numeric = numeric_funs,
  integer = integer_funs,
  factor = factor_funs,
  ordered = factor_funs,
  character = character_funs,
  logical = logical_funs,
  complex = complex_funs,
  date = date_funs,
  Date = date_funs,
  ts = ts_funs,
  POSIXct = posixct_funs,
  list = list_funs
)



# Build environment for storing functions ---------------------------------

functions <- new.env()
functions$default <- .default
functions$current <- .default

#' Set or add the summary functions for a particular type of data
#' 
#' @param ... A list of functions, with an argument name that matches a
#'   particular data type.
#' @param append Whether the provided functions should be in addition to the
#'   defaults already in skim.
#' @export

skim_with <- function(..., append = TRUE) {
  funs <- list(...)
  nms <- purrr::map(funs, names)
  has_null <- purrr::map_lgl(nms, ~any(is.null(.x)))
  if (any(has_null)) {
    msg <- paste(names(funs)[has_null], collapse = ", ")
    stop("A function is missing a name within this type: ", msg)
  }
  all <- purrr::map2(names(funs), funs, set_functions, append)
}

set_functions <- function(type, newfuns, append) {
  exists <- type %in% names(functions$current)
  
  if (!exists) {
    message("Adding new summary functions for type: ", type)
  } else if (append) {
    old <- functions$current[[type]]
    newfuns <- c(old, newfuns)
  }
  
  functions$current[[type]] <- newfuns
}


#' @describeIn skim_with Use the default functions within skim
#' @export

skim_with_defaults <- function() {
  assign("current", .default, envir = functions)
}


#' Show summary functions currently used, by column type
#' 
#' @return Nothing. \code{invisible()}
#' @export

show_skimmers <- function() {
  lapply(functions$current, names)
}


# A method for getting a set of summary functions, by type
#
# @param type The type of summary functions to extract
# @return A list of summary functions
# @keywords internal
# @export

get_funs <- function(type) {
  all <- functions$current[type]
  purrr::detect(all, purrr::compose(`!`, is.null))
}

# A method for getting the name of set of summary functions, by type
#
# @param type The type of summary functions to extract
# @return A list of summary functions
# @keywords internal
# @export

get_fun_names <- function(type) {
  all <- functions$current[type]
  id <- purrr::detect_index(all, purrr::compose(`!`, is.null))
  names(all)[id]
}