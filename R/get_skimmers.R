#' @include stats.R 
NULL

#' Retrieve the summary functions for a specific data type
#' 
#' These functions are used to set the default skimming functions for a data
#' type.
#' 
#' When creating your own set of skimming functions, call [make_skimmers()]
#' within a [get_skimmers()].
#' 
#' @param column An atomic vector or list. A column from a data frame.
#' @param local_skimmers An environment. Used by [skim()].
#' @param append A logical scalar. Used by [skim()]. Whether new skim functions
#'   should be added to the existing set of functions or whether they new
#'   functions should replace the old.
#' @inheritParams dplyr::funs
#' @return A list with two entries:
#' 
#'   - `funs` is a `fun_list`. The same as [dplyr::funs()].
#'   - `type` is the type of skimmer called.
#'
#' @seealso [dplyr::funs()]
#' @export
get_skimmers <- function(column, local_skimmers = new.env(), append = TRUE) {
  UseMethod("get_skimmers")
}

#' @rdname get_skimmers
#' @export
make_skimmer <- function(type, local_skimmers, append, ...) {
  if (append) {
    locals <- local_skimmers[[type]]
    list(funs = merge_skimmers(dplyr::funs(...), locals),
         type = type)
  } else {
    list(funs = local_skimmers[[type]]$keep %||% dplyr::funs(...),
         type = type)
  }
}

merge_skimmers <- function(default, user_provided) {
  if (length(user_provided$keep) > 0) {
    default[names(user_provided$keep)] <- user_provided$keep
  }
  
  if (length(user_provided$drop) > 0) {
    default[user_provided$drop] <- NULL
  }
  
  default
}

#' @export
get_skimmers.default <- function(column, local_skimmers = new.env(),
                                 append = TRUE) {
  all_matches <- local_skimmers[class(column)]
  first_match <- purrr::detect(all_matches, ~!is.null(.x))
  
  if (is.null(first_match)) {
    msg <- "Couldn't find skimmers for class: %s; Using `character`."
    warning(sprintf(msg, class(column)), call. = FALSE)
    get_skimmers(as.character(column), local_skimmers)
  } else {
    dplyr::funs(!!!first_match)
  }
}

#' @export
get_skimmers.double <- function(column, local_skimmers = new.env(),
                                append = TRUE) {
  make_skimmer(
    "double",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = mean(., na.rm = TRUE),
    sd = sd(., na.rm = TRUE),
    p0 = quantile(., probs = 0, na.rm = TRUE, names = FALSE),
    p25 = quantile(., probs = .25, na.rm = TRUE, names = FALSE),
    p50 = quantile(., probs= .50, na.rm = TRUE, names = FALSE),
    p75 = quantile(., probs = .75, na.rm = TRUE, names = FALSE),
    p100 = quantile(., probs = 1, na.rm = TRUE, names = FALSE),
    hist = inline_hist(., 5))
}

#' @export
get_skimmers.integer <- function(column, local_skimmers = new.env(),
                                 append = TRUE) {
  make_skimmer(
    "integer",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = mean(., na.rm = TRUE),
    sd = sd(., na.rm = TRUE),
    p0 = quantile(., probs = 0, na.rm = TRUE, names = FALSE),
    p25 = quantile(., probs = .25, na.rm = TRUE, names = FALSE),
    p50 = quantile(., probs= .50, na.rm = TRUE, names = FALSE),
    p75 = quantile(., probs = .75, na.rm = TRUE, names = FALSE),
    p100 = quantile(., probs = 1, na.rm = TRUE, names = FALSE),
    hist = inline_hist(., 5))
}

#' @export
get_skimmers.factor <- function(column, local_skimmers = new.env(),
                                 append = TRUE) {
  make_skimmer(
    "factor",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    ordered = is.ordered,
    n = length,
    n_unique = n_unique,
    top_counts = top_counts)
}

#' @export
get_skimmers.character <- function(column, local_skimmers = new.env(),
                                   append = TRUE) {
  make_skimmer(
    "character",
    local_skimmers,
    append,
    missing  = n_missing,
    complete = n_complete,
    n = length,
    min = min_char,
    max = max_char,
    empty = n_empty,
    n_unique = n_unique)
}

#' @export
get_skimmers.logical <- function(column, local_skimmers = new.env(),
                                   append = TRUE) {
  make_skimmer(
    "logical",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    mean = mean(., na.rm = TRUE),
    count = sorted_count)
}

#' @export
get_skimmers.complex <- function(column, local_skimmers = new.env(),
                                 append = TRUE) {
  make_skimmer(
    "complex",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length)
}

#' @export
get_skimmers.Date <- function(column, local_skimmers = new.env(),
                              append = TRUE) {
  make_skimmer(
    "date",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    min = min(., na.rm = TRUE),
    max = max(., na.rm = TRUE),
    median = median(., na.rm = TRUE),
    n_unique = n_unique)
}

#' @export
get_skimmers.POSIXct <- function(column, local_skimmers = new.env(),
                                 append = TRUE) {
  
  make_skimmer(
    "posixct",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    min = min(., na.rm = TRUE),
    max = max(., na.rm = TRUE),
    median = median(., na.rm = TRUE),
    n_unique = n_unique)
}

#' @export
get_skimmers.ts <- function(column, local_skimmers = new.env(),
                            append = TRUE) {
  make_skimmer(
    "ts",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    start = ts_start,
    end = ts_end,
    frequency = stats::frequency,
    deltat = stats::deltat,
    mean = mean(., na.rm = TRUE),
    sd = sd(., na.rm = TRUE),
    min = min(., na.rm = TRUE),
    max = max(., na.rm = TRUE),
    median = median(., na.rm = TRUE),
    line_graph  = inline_linegraph(., 16))
}

#' @export
get_skimmers.list <- function(column, local_skimmers = new.env(),
                            append = TRUE) {
  make_skimmer(
    "list",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    n_unique = n_unique,
    min_length= list_min_length,
    max_length = list_max_length)
}

#' @export
get_skimmers.AsIs <- function(column, local_skimmers = new.env(),
                            append = TRUE) {
  make_skimmer(
    "asis",
    local_skimmers,
    append,
    missing = n_missing,
    complete = n_complete,
    n = length,
    n_unique = n_unique,
    min_length= list_min_length,
    max_length = list_max_length)
}
