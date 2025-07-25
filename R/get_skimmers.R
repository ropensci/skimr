#' @include skim_with.R stats.R
NULL

#' Retrieve the summary functions for a specific data type
#'
#' These functions are used to set the default skimming functions for a data
#' type. They are combined with the base skim function list (`sfl`) in
#' [skim_with()], to create the summary tibble for each type.
#'
#' When creating your own set of skimming functions, call [sfl()] within a
#' [get_skimmers()] method for your particular type. Your call to [sfl()] should
#' also provide a matching class in the `skim_type` argument.  Otherwise, it
#' will not be possible to dynamically reassign your default functions when
#' working interactively.
#'
#' Call [get_default_skimmers()] to see the functions for each type of summary
#' function currently supported. Call [get_default_skimmer_names()] to just see
#' the names of these functions. Use [modify_default_skimmers()] for a method
#' for changing the `skim_type` or functions for a default `sfl`. This is useful
#' for creating new default `sfl`'s.
#'
#' @param column An atomic vector or list. A column from a data frame.
#' @param skim_type A character scalar. The class of the object with default
#'   skimmers.
#' @return A `skim_function_list` object.
#' @seealso [sfl()]
#' @examples
#' # Defining default skimming functions for a new class, `my_class`.
#' # Note that the class argument is required for dynamic reassignment.
#' get_skimmers.my_class <- function(column) {
#'   sfl(
#'     skim_type = "my_class",
#'     mean,
#'     sd
#'   )
#' }
#'
#' # Integer and double columns are both "numeric" and are treated the same
#' # by default. To switch this behavior in another package, add a method.
#' get_skimmers.integer <- function(column) {
#'   sfl(
#'     skim_type = "integer",
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
#' # In a package, to revert to the V1 behavior of skimming separately with the
#' # same functions, assign the numeric `get_skimmers`.
#' get_skimmers.integer <- skimr::get_skimmers.numeric
#'
#' # Or, in a local session, use `skim_with` to create a different `skim`.
#' new_skim <- skim_with(integer = skimr::get_skimmers.numeric())
#'
#' # To apply a set of skimmers from an old type to a new type
#' get_skimmers.new_type <- function(column) {
#'   modify_default_skimmers("old_type", new_skim_type = "new_type")
#' }
#' }
#' @export
get_skimmers <- function(column) {
  UseMethod("get_skimmers")
}

#' @describeIn get_skimmers The default method for skimming data. Only used when
#'   a column's data type doesn't match currently installed types. Call
#'   [get_default_skimmer_names] to see these defaults.
#' @export
get_skimmers.default <- function(column) {
  modify_default_skimmers("character", new_skim_type = "default")
}

#' @describeIn get_skimmers Summary functions for numeric columns, covering both
#'   [double()] and [integer()] classes: [mean()], [sd()], [quantile()] and
#'   [inline_hist()].
#' @export
get_skimmers.numeric <- function(column) {
  sfl(
    skim_type = "numeric",
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

#' @describeIn get_skimmers Summary functions for factor columns:
#'   [is.ordered()], [n_unique()] and [top_counts()].
#' @export
get_skimmers.factor <- function(column) {
  sfl(
    skim_type = "factor",
    ordered = is.ordered,
    n_unique = n_unique,
    top_counts = top_counts
  )
}

#' @describeIn get_skimmers Summary functions for character columns. Also, the
#'   default for unknown columns: [min_char()], [max_char()], [n_empty()],
#'   [n_unique()] and [n_whitespace()].
#' @export
get_skimmers.character <- function(column) {
  sfl(
    skim_type = "character",
    min = min_char,
    max = max_char,
    empty = n_empty,
    n_unique = n_unique,
    whitespace = n_whitespace
  )
}

#' @describeIn get_skimmers Summary functions for logical/ boolean columns:
#'   [mean()], which produces rates for each value, and [top_counts()].
#' @export
get_skimmers.logical <- function(column) {
  sfl(
    skim_type = "logical",
    mean = ~ mean(., na.rm = TRUE),
    count = top_counts
  )
}

#' @describeIn get_skimmers Summary functions for complex columns: [mean()].
#' @export
get_skimmers.complex <- function(column) {
  sfl(
    skim_type = "complex",
    mean = ~ mean(., na.rm = TRUE)
  )
}

#' @describeIn get_skimmers Summary functions for `Date` columns: [min()],
#'   [max()], [median()] and [n_unique()].
#' @export
get_skimmers.Date <- function(column) {
  sfl(
    skim_type = "Date",
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    median = ~ stats::median(., na.rm = TRUE),
    n_unique = n_unique
  )
}

#' @describeIn get_skimmers Summary functions for `POSIXct` columns: [min()],
#'   [max()], [median()] and [n_unique()].
#' @export
get_skimmers.POSIXct <- function(column) {
  modify_default_skimmers("Date", new_skim_type = "POSIXct")
}

#' @describeIn get_skimmers Summary functions for `difftime` columns: [min()],
#'   [max()], [median()] and [n_unique()].
#' @export
get_skimmers.difftime <- function(column) {
  modify_default_skimmers("Date", new_skim_type = "difftime")
}

#' @describeIn get_skimmers Summary functions for `Timespan` columns: [min()],
#'   [max()], [median()] and [n_unique()].
#' @export
get_skimmers.Timespan <- function(column) {
  modify_default_skimmers("difftime", new_skim_type = "Timespan")
}

#' @describeIn get_skimmers Summary functions for `ts` columns: [min()],
#'   [max()], [median()] and [n_unique()].
#' @export
get_skimmers.ts <- function(column) {
  sfl(
    skim_type = "ts",
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

#' @describeIn get_skimmers Summary functions for `list` columns: [n_unique()],
#'  [list_min_length()] and [list_max_length()].
#' @export
get_skimmers.list <- function(column) {
  sfl(
    skim_type = "list",
    n_unique = n_unique,
    min_length = list_min_length,
    max_length = list_max_length
  )
}

#' @describeIn get_skimmers Summary functions for `AsIs` columns: [n_unique()],
#'  [list_min_length()] and [list_max_length()].
#' @export
get_skimmers.AsIs <- function(column) {
  modify_default_skimmers("list", new_skim_type = "AsIs")
}

#' @describeIn  get_skimmers Summary functions for `haven_labelled` columns.
#'  Finds the appropriate skimmers for the underlying data in the vector.
#' @export
get_skimmers.haven_labelled <- function(column) {
  stopifnot(requireNamespace("haven", quietly = TRUE))
  get_skimmers(vctrs::vec_data(column))
}

#' @rdname get_skimmers
#' @param new_skim_type The type to assign to the looked up set of skimmers.
#' @param new_funs Replacement functions for those in
#' @export
modify_default_skimmers <- function(skim_type,
                                    new_skim_type = NULL,
                                    new_funs = list()) {
  funs <- get_one_default_skimmer(skim_type)
  new_funs <- purrr::list_modify(funs, !!!new_funs)
  sfl(skim_type = new_skim_type %||% skim_type, !!!new_funs)
}

#' View default skimmer names and functions
#'
#' These utility functions look up the currently-available defaults for one or
#' more `skim_type`'s. They work with all defaults in the `skimr` package, as
#' well as the defaults in any package that extends `skimr`. See
#' [get_skimmers()] for writing lookup methods for different.
#'
#' The functions differ in return type and whether or not the result is in
#' a list. [get_default_skimmers()] and [get_one_default_skimmer()] return
#' functions. The former returns functions within a typed list, i.e.
#' `list(numeric = list(...functions...))`.
#'
#' The functions differ in return type and whether or not the result is in
#' a list. [get_default_skimmer_names()] and [get_one_default_skimmer_names()]
#' return a list of character vectors or a single character vector.
#'
#' [get_sfl()] returns the skimmer function list for a particular `skim_type`.
#' It differs from [get_default_skimmers()] in that the returned `sfl` contains
#' a list of functions and a `skim_type`.
#'
#' @param skim_type The class of the column being skimmed.
#' @export
get_default_skimmers <- function(skim_type = NULL) {
  if (is.null(skim_type)) {
    defaults <- as.character(utils::methods("get_skimmers"))
    types <- stringr::str_replace(defaults, "get_skimmers.", "")
    no_default <- purrr::discard(types, ~ .x == "default")
    iter <- rlang::set_names(no_default)
  } else {
    iter <- rlang::set_names(skim_type)
  }

  results <- purrr::map(iter, get_one_default_skimmer)
  no_defaults_for_class <- purrr::map_lgl(results, anyNA)
  results[!no_defaults_for_class]
}

#' @describeIn get_default_skimmers Get the functions associated with one
#'   `skim_type`.
#' @export
get_one_default_skimmer <- function(skim_type) {
  skimmers <- get_sfl(skim_type)
  skimmers$funs
}

#' @describeIn get_default_skimmers Get the names of the functions used in one
#'   or more `skim_type`'s.
#' @export
get_default_skimmer_names <- function(skim_type = NULL) {
  skimmers <- get_default_skimmers(skim_type)
  purrr::map(skimmers, names)
}

#' @describeIn get_default_skimmers Get the names of the functions used in one
#'   `skim_type`.
#' @export
get_one_default_skimmer_names <- function(skim_type) {
  skimmers <- get_one_default_skimmer(skim_type)
  names(skimmers)
}

#' @describeIn get_default_skimmers Get the `sfl` for a `skim_type`.
#' @export
get_sfl <- function(skim_type) {
  skimmers <- get_skimmers(structure(integer(), class = skim_type))
  if (skimmers$skim_type == "default") {
    warning("There are no default skimmers for type: ", skim_type)
    sfl(NA, skim_type = NA_character_)
  } else {
    skimmers
  }
}
