#' @include skim_with.R stats.R
NULL

#' Retrieve the summary functions for a specific data type
#'
#' These functions are used to set the default skimming functions for a data
#' type.  When creating your own set of skimming functions, call [sfl()]
#' within a [get_skimmers()] method for your particular type. Your call to
#' [sfl()] should also provide a matching class in the `.type` argument.
#' Otherwise, it will not be possible to dynamically reassign your default
#' functions when working interactively.
#'
#' Summary functions are provided for the following classes:
#'
#'  - `numeric`
#'  - `character`
#'  - `factor`
#'  - `logical`
#'  - `complex`
#'  - `Date`
#'  - `POSIXct`
#'  - `difftime`
#'  - `ts`
#'  - `AsIs`
#'
#' Call [get_default_skimmers()] to see the functions for each.
#'
#' @param column An atomic vector or list. A column from a data frame.
#' @return A `skim_function_list` object.
#' @seealso [sfl()]
#' @examples
#' # Defining default skimming functions for a new class, `my_class`.
#' # Note that the class argument is required for dynamic reassignment.
#' get_skimmers.my_class <- function(column) {
#'   sfl(
#'     .type = "my_class",
#'     mean,
#'     sd
#'   )
#' }
#'
#' # Integer and float columns are both "numeric" and are treated the same
#' # by default. To switch this behavior in another package, add a method.
#' get_skimmers.integer <- function(column) {
#'   sfl(
#'     .type = "integer",
#'     p50 = ~ stats::quantile(
#'       .,
#'       probs = .50, na.rm = TRUE, names = FALSE, type = 1
#'     )
#'   )
#' }
#' x <- mtcars[c("gear", "carb")]
#' class(x$carb) <- "integer"
#' skim(x)
#' \dontrun{
#'   # In a package, to revert to the V1 behavior of skimming separately with the
#'   # same functions, assign the numeric `get_skimmers`.
#'   get_skimmers.integer <- skimr::get_skimmers.numeric
#'
#'   # Or, in a local session, use `skim_with` to create a different `skim`.
#'   new_skim <- skim_with(integer = skimr::get_skimmers.numeric())
#' }
#' @export
get_skimmers <- function(column) {
  UseMethod("get_skimmers")
}

#' @export
get_skimmers.default <- function(column) {
  fallback <- get_skimmers(character())
  sfl(.type = "default", !!!fallback$keep)
}

#' @export
get_skimmers.numeric <- function(column) {
  sfl(
    .type = "numeric",
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ stats::sd(., na.rm = TRUE),
    p0 = ~ stats::quantile(., probs = 0, na.rm = TRUE, names = FALSE),
    p25 = ~ stats::quantile(., probs = .25, na.rm = TRUE, names = FALSE),
    p50 = ~ stats::quantile(., probs = .50, na.rm = TRUE, names = FALSE),
    p75 = ~ stats::quantile(., probs = .75, na.rm = TRUE, names = FALSE),
    p100 = ~ stats::quantile(., probs = 1, na.rm = TRUE, names = FALSE),
    hist = ~ inline_hist(., 5)
  )
}

#' @export
get_skimmers.factor <- function(column) {
  sfl(
    .type = "factor",
    missing = n_missing,
    complete = n_complete,
    n = length,
    ordered = is.ordered,
    n_unique = n_unique,
    top_counts = top_counts
  )
}

#' @export
get_skimmers.character <- function(column) {
  sfl(
    .type = "character",
    missing = n_missing,
    complete = n_complete,
    n = length,
    min = min_char,
    max = max_char,
    empty = n_empty,
    n_unique = n_unique,
    whitespace = n_whitespace
  )
}

#' @export
get_skimmers.logical <- function(column) {
  sfl(
    .type = "logical",
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = ~ mean(., na.rm = TRUE),
    count = top_counts
  )
}

#' @export
get_skimmers.complex <- function(column) {
  sfl(
    .type = "complex",
    missing = n_missing,
    complete = n_complete,
    n = length
  )
}

#' @export
get_skimmers.Date <- function(column) {
  sfl(
    .type = "Date",
    missing = n_missing,
    complete = n_complete,
    n = length,
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    median = ~ stats::median(., na.rm = TRUE),
    n_unique = n_unique
  )
}

#' @export
get_skimmers.POSIXct <- function(column) {
  date_skimmers <- get_skimmers(structure(list(), class = "Date"))
  sfl(.type = "POSIXct", !!!date_skimmers$keep)
}

#' @export
get_skimmers.difftime <- function(column) {
  date_skimmers <- get_skimmers(structure(list(), class = "Date"))
  sfl(.type = "difftime", !!!date_skimmers$keep)
}

#' @export
get_skimmers.ts <- function(column) {
  sfl(
    .type = "ts",
    missing = n_missing,
    complete = n_complete,
    n = length,
    start = ts_start,
    end = ts_end,
    frequency = stats::frequency,
    deltat = stats::deltat,
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ stats::sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    median = ~ stats::median(., na.rm = TRUE),
    line_graph = ~ inline_linegraph(., 16)
  )
}

#' @export
get_skimmers.list <- function(column) {
  sfl(
    .type = "list",
    missing = n_missing,
    complete = n_complete,
    n = length,
    n_unique = n_unique,
    min_length = list_min_length,
    max_length = list_max_length
  )
}

#' @export
get_skimmers.AsIs <- function(column) {
  list_skimmers <- get_skimmers(list())
  sfl(.type = "AsIs", !!!list_skimmers$keep)
}

#' @rdname get_skimmers
#' @param class The class of the column being skimmed
#' @export
get_default_skimmers <- function(class = NULL) {
  if (is.null(class)) {
    defaults <- as.character(utils::methods("get_skimmers"))
    classes <- stringr::str_replace(defaults, "get_skimmers.", "")
    no_default <- purrr::discard(classes, ~ .x == "default")
    iter <- purrr::set_names(no_default)
    purrr::map(iter, get_class_defaults)
  } else {
    iter <- purrr::set_names(class)
    results <- purrr::map(iter, get_class_defaults)
    no_defaults_for_class <- purrr::map_lgl(results, anyNA)
    if (any(no_defaults_for_class)) {
      dropped <- paste(results[no_defaults_for_class], collapse = ", ")
      msg <- sprintf("The following classes do not have defaults: %s", dropped)
      warning(msg, call. = FALSE)
    }
    results[!no_defaults_for_class]
  }
}

get_class_defaults <- function(class) {
  skimmers <- get_skimmers(structure(integer(), class = class))
  if (skimmers$type == "default") {
    NA
  } else {
    names(skimmers$keep)
  }
}
