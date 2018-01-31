globalVariables(".")

#' Skim a data frame, getting useful summary statistics
#' 
#' `skim()` is an alternative to [`summary()`], quickly providing a broad
#' overview of a data frame. It handles data of all types, dispatching a
#' different set of summary functions based on the types of columns in the data
#' frame.
#' 
#' Each call produces a `skim_df`, which is a fundamentally a tibble with a
#' special print method. Instead of showing the result in a long format, `skim`
#' prints a wide version of your data with formatting applied to each column.
#' Printing does not change the structure of the `skim_df`, which remains a long
#' tibble.
#' 
#' If you just want to see the printed output, call `skim_tee()` instead.
#' This function returns the original data frame. If you want to work with
#' a data frame that resembles the printed output, call [`skim_to_wide()`] or
#' [`skim_to_list()`]. Note that all of the columns in the data frames produced
#' by these functions are character. The intent is that you will be processing
#' the **printed** result further, not the original data.
#' 
#' `skim()` is designed to operate in pipes and to generally play nicely with
#' other `tidyverse` functions. This means that you can use `tidyselect` helpers
#' within `skim` to select or drop specific columns for summary. You can also
#' further work with a `skim_df` using `dplyr` functions in a pipeline.
#' 
#' @section Customizing skim:
#' `skim()` is an intentionally simple function, with minimal arguments like
#' [`summary()`]. Nonetheless, this package provides two broad approaches to
#' how you can customize `skim()`'s behavior. You can customize the functions
#' that are called to produce summary statistics with [`skim_with()`]. You
#' can customize how the output is displayed with [`skim_format()`].
#'
#' @section Unicode rendering:
#' If the rendered examples show unencoded values such as `<U+2587>` you will
#' need to change your locale to allow proper rendering. Please review the
#' *Using Skimr* vignette for more information
#' (`vignette("Using_skimr" package = "skimr")`).
#'
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Additional options, normally used to list individual unquoted
#'   column names.
#' @return A `skim_df` object, which can be treated like a tibble in most
#'   instances.
#' @examples
#' skim(iris)
#'
#' # Use tidyselect
#' skim(iris, Species)
#' skim(iris, starts_with("Sepal"))
#' 
#' # Skim also works groupwise
#' dplyr::group_by(iris) %>% skim()
#' 
#' # Skim pipelines; now we work with the tall format
#' skim(iris) %>% as.data.frame()
#' skim(iris) %>% dplyr::filter(type == "factor")
#' 
#' # Which column as the greatest mean value?
#' skim(iris) %>%
#'   dplyr::filter(stat == "mean") %>%
#'   dplyr::arrange(dplyr::desc(value))
#' @export

skim <- function(.data, ...) {
  UseMethod("skim")
}

#'@export

skim.data.frame <- function(.data, ... ) {
  .vars <- rlang::quos(...)
  if (length(.vars) == 0)  selected <- tidyselect::everything(.data)
  else  selected <- tidyselect::vars_select(names(.data), !!! .vars) 

  rows <- purrr::map(.data[selected], skim_v)
  combined <- dplyr::bind_rows(rows, .id = "variable")
  structure(combined, class = c("skim_df", class(combined)),
            data_rows = nrow(.data), data_cols = ncol(.data), 
            df_name = substitute(.data))
}

#' @export

skim.grouped_df <- function(.data, ...) {
  defaults <- options(dplyr.show_progress = FALSE)
  on.exit(options(defaults))
  skimmed <- dplyr::do(.data, skim(., ...))
  
  # Drop the grouping variable
  groups <- dplyr::groups(skimmed)
  to_drop <- quote(!(variable %in% groups))
  skimmed <- dplyr::filter(skimmed, !!to_drop)
  structure(skimmed, class = c("skim_df", class(skimmed)),
            data_rows = nrow(.data), data_cols = ncol(.data), 
            df_name = substitute(.data))
}

#' @export

skim.default <-function(.data, ...){
  if (!is.atomic(.data) | !is.null(dim(.data))[1]){
    return(message("No skim method exists for class ", class(.data), "."))
  }
  skimmed <- skim_v(.data)
  skimmed$variable <- deparse(substitute(.data))
  skimmed <- dplyr::select(skimmed, !!rlang::sym("variable"), 
                           !!rlang::sym("type"), 
                           !!rlang::sym("stat"), 
                           !!rlang::sym("level"), 
                           !!rlang::sym("value"), 
                           !!rlang::sym("formatted"))
  structure(skimmed, class = c("skim_vector", "skim_df", class(skimmed)),
            df_name = skimmed$variable[1])
}


#' @rdname skim 
#' @export

skim_tee <- function(.data, ...) {
  print(skim(.data))
  invisible(.data)
}
