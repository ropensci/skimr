#' Skim a data frame, getting useful summary statistics
#'
#' `skim()` is an alternative to [summary()], quickly providing a broad
#' overview of a data frame. It handles data of all types, dispatching a
#' different set of summary functions based on the types of columns in the data
#' frame.
#'
#' If you just want to see the printed output, call `skim_tee()` instead.
#' This function returns the original data. `skim_tee()` uses the default
#' `skim()`, but you can replace it with the `skim` argument.
#'
#' The data frame produced by `skim` is wide and sparse. To avoid type coercion
#' `skimr` uses a type namespace for all summary statistics. Columns for numeric
#' summary statistics all begin `numeric`; for factor summary statistics
#' begin `factor`; and so on.
#'
#' See [partition()] and [yank()] for methods for transforming this wide data
#' frame. The first function splits it into a list, with each entry
#' corresponding to a data type. The latter pulls a single subtable for a
#' particular type from the `skim_df`.
#'
#' `skim()` is designed to operate in pipes and to generally play nicely with
#' other `tidyverse` functions. This means that you can use `tidyselect` helpers
#' within `skim` to select or drop specific columns for summary. You can also
#' further work with a `skim_df` using `dplyr` functions in a pipeline.
#'
#' @section Customizing skim:
#' `skim()` is an intentionally simple function, with minimal arguments like
#' [summary()]. Nonetheless, this package provides two broad approaches to
#' how you can customize `skim()`'s behavior. You can customize the functions
#' that are called to produce summary statistics with [skim_with()].
#'
#' @section Unicode rendering:
#' If the rendered examples show unencoded values such as `<U+2587>` you will
#' need to change your locale to allow proper rendering. Please review the
#' *Using Skimr* vignette for more information
#' (`vignette("Using_skimr", package = "skimr")`).
#'
#' Otherwise, we export `skim_without_charts()` to produce summaries without the
#' spark graphs. These are the source of the unicode dependency.
#'
#' @param data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Columns to select for skimming. When none are provided, the
#'   default is to skim all columns.
#' @param skim  The skimming function to use in `skim_tee()`.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. In many ways, the object behaves like a [tibble::tibble()].
#' @examples
#' skim(iris)
#'
#' # Use tidyselect
#' skim(iris, Species)
#' skim(iris, starts_with("Sepal"))
#'
#' # Skim also works groupwise
#' iris %>%
#'   dplyr::group_by(Species) %>%
#'   skim()
#'
#' # Which five numeric columns have the greatest mean value?
#' iris %>%
#'   skim() %>%
#'   dplyr::select(numeric.mean) %>%
#'   dplyr::top_n(5)
#'
#' # Use skim_tee to view the skim results and
#' # continue using the original data.
#' chickwts %>%
#'   skim_tee() %>%
#'   dplyr::filter(feed == "sunflower")
#'
#' # Produce a summary without spark graphs
#' iris %>%
#'   skim_without_charts()
#' @export
skim <- skim_with()

#' @rdname skim
#' @param skim_fun The skim function used.
#' @export
skim_tee <- function(data, ..., skim_fun = skim) {
  skimmed <- skim_fun(data, ...)
  print(skimmed)
  invisible(data)
}

#' @rdname skim
#' @export
skim_without_charts <- skim_with(
  numeric = sfl(hist = NULL),
  ts = sfl(line_graph = NULL)
)
