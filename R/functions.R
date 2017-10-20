#' @include skimr-package.R stats.R
NULL

#' Set or add the summary functions for a particular type of data
#' 
#' While skim is designed around having an opinionated set of defaults, you
#' can use this function to change the summary statistics that it returns.
#' To do that, provide type you wish to change as an argument to this function,
#' along with a list of named functions that you want to use instead of the
#' defaults. The \code{append} argument lets you decide whether you want to
#' replace the defaults or add to them.
#' 
#' This function is not pure. It sets values in within the package environment.
#' This is an intentional design choice, with effects similar to setting
#' options in base R. By setting options here for your entire session, you
#' can continue to summarize using skim on its own.
#' 
#' @param ... A list of functions, with an argument name that matches a
#'  particular data type.
#' @param append Whether the provided options should be in addition to the
#'  defaults already in skim for the given types specified by the named
#'  arugments in \code{...}. Default is \code{TRUE}.
#' @return Nothing. \code{invisible(NULL)}
#' @examples
#' # Use new functions for numeric functions
#' skim_with(numeric = list(median = median, mad = mad), append = FALSE)
#' skim(faithful)
#' 
#' # If you want to remove a particular skimmer, set it to NULL
#' # This removes the inline histogram
#' skim_with(numeric = list(hist = NULL))
#' skim(faithful)
#' 
#' # Go back to defaults
#' skim_with_defaults()
#' skim(faithful)
#' @export

skim_with <- function(..., append = TRUE) {
  skim_options(..., env = "functions", append = append)
}


#' @describeIn skim_with Use the default functions within skim
#' @export

skim_with_defaults <- function() {
  assign("functions", .default, envir = options)
}


#' Show summary functions currently used, by data type
#' 
#' The names of the summary functions are stored as a character vector within
#' a list. The names of the values in the list match the names of the classes
#' that have assigned sets of summary functions.
#' 
#' @param which A character vector. One or more of the classes whose summary
#'  functions you wish to display.
#' @return A list. The names of the list match the classes that have assigned
#'  summary functions.
#' @export

show_skimmers <- function(which = NULL) {
  show_options(which, "functions")
}


#' An internal method for getting a set of summary functions, by type. We use
#' this approach instead of method dispatch because we want to be able to
#' dynamically add or remove the summary functions for each type.
#'
#' The call to purrr::detect returns the first appropriate match for objects
#' that have more than one class. Doing it this way saves us from having
#' to define methods for multiple classes.
#'
#' @param type The type of summary functions to extract
#' @return A list of summary functions
#' @keywords internal
#' @noRd

get_funs <- function(type) {
  all <- options$functions[type]
  purrr::detect(all, purrr::compose(`!`, is.null))
}


#' A method for getting the name of set of summary functions names, by type.
#' This function is similar to get_funs, in that it applies a type of dispatch
#' for the type of object provided. This function gets the name of the
#' group of summary functions for the type. This lets the user know what set of
#' functions are used in producing the skim_df.
#'
#' @param type The type of summary functions to extract
#' @return A length-one character vector that shows the class that was matched
#'  by skimr.
#' @keywords internal
#' @noRd

get_vector_type_used <- function(type) {
  all <- options$functions[type]
  id <- purrr::detect_index(all, purrr::compose(`!`, is.null))
  names(all)[id]
}


# Default summarizing functions for each type -----------------------------

numeric_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  mean = purrr::partial(mean, na.rm = TRUE),
  sd = purrr::partial(sd, na.rm = TRUE),
  min = purrr::partial(min, na.rm = TRUE),
  p25 = purrr::partial(quantile, probs = .25, na.rm = TRUE, names = FALSE),
  median = purrr::partial(median, na.rm = TRUE),
  p75 = purrr::partial(quantile, probs = .75, na.rm = TRUE, names = FALSE),
  max = purrr::partial(max, na.rm = TRUE),
  hist = inline_hist
)

factor_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = n_unique,
  top_counts = sorted_count,
  ordered = is.ordered
)

character_funs <- list (
  missing  = n_missing,
  complete = n_complete,
  n = length,
  min = min_char,
  max = max_char,
  empty = n_empty,
  n_unique = n_unique
)

logical_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  mean = purrr::partial(mean, na.rm = TRUE),
  count = sorted_count
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
  n_unique = n_unique
)

ts_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  start = ts_start,
  end = ts_end,
  frequency = stats::frequency,
  deltat = stats::deltat,
  mean = purrr::partial(mean, na.rm = TRUE),
  sd = purrr::partial(sd, na.rm = TRUE),
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  line_graph  = inline_linegraph
)

posixct_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  n_unique = n_unique 
)

asis_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = n_unique,
  min_length= list_min_length,
  max_length = list_max_length
)

list_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = n_unique,
  min_length = list_lengths_min,
  median_length = list_lengths_median,
  max_length = list_lengths_max
)

hms_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  n_unique = n_unique
)

.default <- list(
  numeric = numeric_funs,
  integer = integer_funs,
  factor = factor_funs,
  character = character_funs,
  logical = logical_funs,
  complex = complex_funs,
  date = date_funs,
  Date = date_funs,
  ts = ts_funs,
  POSIXct = posixct_funs,
  list = list_funs,
  AsIs = asis_funs,
  hms = hms_funs
)
# Set the default skimming functions
options$functions <- .default
