#' @include skimr-package.R stats.R
NULL

#' Set or add the summary functions for a particular type of data
#' 
#' While skim is designed around having an opinionated set of defaults, you
#' can use these functions to change the summary statistics that it returns.
#' To do that, provide type you wish to change as an argument to this function,
#' along with a list of named functions that you want to use instead of the
#' defaults. 
#' 
#' This function is not pure. It sets values in within the package environment.
#' This is an intentional design choice, with effects similar to setting
#' options in base R. By setting options here for your entire session, you
#' can continue to summarize using skim on its own.
#' 
#' @param ... A list of functions, with an argument name that matches a
#'  particular data type.
#' @param .list Instead of individual named entries, you can provided a list
#'  instead. If most \code{...} arguments and \code{.list} is provided, values
#'  in \code{.list} take precedence.
#' @param append Whether the provided options should be in addition to the
#'  defaults already in `skim`. Default is `TRUE`.
#' @return When setting values, `invisible(NULL)`. When looking up values a
#'  list. The names of the list match the classes that have assigned
#'  summary functions. With [`show_skimmers()`], each entry in the list is a
#'  character vector of function names. With [`get_skimmers()`], each entry
#'  in the list is itself a list of named functions.
#' @param drop_new Whether types outside of the defaults should be discarded.
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
#' # This works with multiple skimmers. Just match names to overwrite
#' skim_with(numeric = list(iqr = IQR, p25 = NULL, p75 = NULL))
#' skim(faithful)
#' 
#' # Alternatively, set `append = FALSE` to replace the skimmers of a type.
#' skim_with(numeric = list(mean = mean, sd = sd), append = FALSE)
#' 
#' # Skimmers are unary functions. Partially apply arguments during assigment.
#' # For example, you might want to remove NA values.
#' skim_with(numeric = list(iqr = purrr::partial(IQR, na.rm = TRUE)))
#' 
#' # Or use an rlang-style formula constructor for the function
#' skim_with(numeric = list(med = ~median(., na.rm = TRUE)))
#' 
#' # Set multiple types of skimmers simultaneously
#' skim_with(numeric = list(mean = mean), character = list(len = length))
#' 
#' # Or pass the same as a list
#' my_skimmers <- list(numeric = list(mean = mean),
#'                     character = list(len = length))
#' skim_with(.list = my_skimmers)
#' 
#' # Alternatively, use rlang unquoting semantics
#' skim_with(!!!my_skimmers)
#' 
#' # Go back to defaults
#' skim_with_defaults()
#' skim(faithful)
#' 
#' # What are the names of the numeric skimmers?
#' show_skimmers("numeric")
#' 
#' # I want to create a set of skimmers for the hms class, using the date
#' # skimmers currently available.
#' funs <- get_skimmers()
#' skim_with(hms = funs$date)
#' @export

skim_with <- function(..., .list = list(), append = TRUE, drop_new = FALSE) {
  combined <- purrr::list_modify(rlang::dots_list(...), !!! .list)
  skimmers <- purrr::modify_depth(combined, 1, purrr::map_if,
                                  Negate(is.null), rlang::as_function)
  skim_options(skimmers, env = "functions", append = append,
               drop_new = drop_new)
}


#' @describeIn skim_with Use the default functions within skim
#' @export

skim_with_defaults <- function() {
  skim_with(.list = .default, append = FALSE, drop_new = TRUE)
}


#' @describeIn skim_with Access the names of the summary functions for a
#'   class.
#' @param which A character vector. One or more of the classes whose summary
#'  functions you wish to display.
#' @export

show_skimmers <- function(which = NULL) {
  show_options(which, "functions", only_names = TRUE)
}

#' @describeIn skim_with Pull a list of summary functions for a class.
#' @export

get_skimmers <- function(which = NULL) {
  show_options(which, "functions", only_names = FALSE)
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

# nocov start

numeric_funs <- NULL
factor_funs <- NULL
character_funs <- NULL
logical_funs <- NULL
integer_funs <- NULL
complex_funs <- NULL
date_funs <- NULL
ts_funs <- NULL
posixct_funs <- NULL
asis_funs <- NULL
list_funs <- NULL
difftime_funs <- NULL
.default <- NULL

# These functions must be copied at load time rather than build
# time. Otherwise we partially copy their internals at the time of
# build time (their body()) into our namespace. This is only partial
# because the namespaces they inherit from is not copied. If a package
# is updated in a way that is incompatible with these bodies, we'll
# get bugs until the package is rebuilt and reinstalled.
init_functions <- function() {
  numeric_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = purrr::partial(mean.default, na.rm = TRUE),
    sd = purrr::partial(stats::sd, na.rm = TRUE),
    p0 = purrr::partial(stats::quantile, probs = 0, na.rm = TRUE, names = FALSE),
    p25 = purrr::partial(stats::quantile, probs = .25, na.rm = TRUE, names = FALSE),
    p50 = purrr::partial(stats::quantile, probs= .50, na.rm = TRUE, names = FALSE),
    p75 = purrr::partial(stats::quantile, probs = .75, na.rm = TRUE, names = FALSE),
    p100 = purrr::partial(stats::quantile, probs = 1, na.rm = TRUE, names = FALSE),
    hist = inline_hist
  )

  factor_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    n_unique = n_unique,
    top_counts = sorted_count,
    ordered = is.ordered
  )

  character_funs <<- list (
    missing  = n_missing,
    complete = n_complete,
    n = length,
    min = min_char,
    max = max_char,
    empty = n_empty,
    n_unique = n_unique
  )

  logical_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = purrr::partial(mean.default, na.rm = TRUE),
    count = sorted_count
  )

  integer_funs <<- numeric_funs

  complex_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length
  )

  date_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    min = purrr::partial(min, na.rm = TRUE),
    max = purrr::partial(max, na.rm = TRUE),
    median = purrr::partial(stats::median, na.rm = TRUE),
    n_unique = n_unique
  )

  ts_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    start = ts_start,
    end = ts_end,
    frequency = stats::frequency,
    deltat = stats::deltat,
    mean = purrr::partial(mean.default, na.rm = TRUE),
    sd = purrr::partial(stats::sd, na.rm = TRUE),
    min = purrr::partial(min, na.rm = TRUE),
    max = purrr::partial(max, na.rm = TRUE),
    median = purrr::partial(stats::median, na.rm = TRUE),
    line_graph  = inline_linegraph
  )

  posixct_funs <<- date_funs

  asis_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    n_unique = n_unique,
    min_length= list_min_length,
    max_length = list_max_length
  )

  list_funs <<- list(
    missing = n_missing,
    complete = n_complete,
    n = length,
    n_unique = n_unique,
    min_length = list_lengths_min,
    median_length = list_lengths_median,
    max_length = list_lengths_max
  )

  difftime_funs <<- date_funs

  .default <<- list(
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
    difftime = difftime_funs
  )

  # Set the default skimming functions
  options$functions <- .default
}

# nocov end
