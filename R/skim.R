#' Set or add the summary functions for a particular type of data
#' 
#' While skim is designed around having an opinionated set of defaults, you
#' can use these functions to change the summary statistics that it returns.
#' To do that, provide type you wish to change as an argument to this function,
#' along with a list of named functions that you want to use instead of the
#' defaults. 
#' 
#' `skim_with()` is a closure: a function that returns a new function. This
#' lets you have several skimming functions in a single R session, but it
#' also means that you need to assign the return of `skim_with()` before
#' you can use it.
#' 
#' You assign values within `skim_with` by using the [sfl()] helper (`skimr`
#' function list). This helper behaves mostly like [dplyr::funs()], but lets
#' you also identify which skimming functions you want to remove, by setting
#' them to `NULL`.
#' 
#' @param ... One or more `skimmer_function_list` objects, with an argument name
#'  that matches a particular data type.
#' @param append Whether the provided options should be in addition to the
#'  defaults already in `skim`. Default is `TRUE`.
#' @return A new `skim()` function. This is callable. See [skim()] for
#'  more details.
#' @examples
#' # Use new functions for numeric functions. If you don't provide a name,
#' # one will be automatically generated.
#' my_skim <- skim_with(numeric = sfl(median, mad), append = FALSE)
#' my_skim(faithful)
#' 
#' # If you want to remove a particular skimmer, set it to NULL
#' # This removes the inline histogram
#' my_skim <- skim_with(numeric = sfl(hist = NULL))
#' my_skim(faithful)
#' 
#' # This works with multiple skimmers. Just match names to overwrite
#' my_skim <- skim_with(numeric = sfl(iqr = IQR, p25 = NULL, p75 = NULL))
#' my_skim(faithful)
#' 
#' # Alternatively, set `append = FALSE` to replace the skimmers of a type.
#' my_skim <- skim_with(numeric = sfl(mean = mean, sd = sd), append = FALSE)
#' 
#' # Skimmers are unary functions. Partially apply arguments during assigment.
#' # For example, you might want to remove NA values. Use `dplyr::funs()`
#' # syntax for partial application.
#' my_skim <- skim_with(numeric = sfl(iqr = IQR(., na.rm = TRUE)))
#' 
#' # Or, use the `.args` argument from `dplyr::funs()`
#' my_skim <- skim_with(numeric = sfl(median, mad, .args = list(na.rm = FALSE)))
#' 
#' # Set multiple types of skimmers simultaneously.
#' skim2 <- skim_with(numeric = sfl(mean), character = sfl(length))
#' 
#' # Or pass the same as a list
#' my_skimmers <- list(numeric = sfl(mean), character = sfl(length))
#' my_skim <- skim_with(!!!my_skimmers)
#' @export
skim_with <- function(..., append = TRUE) {
  local_skimmers <- validate_assignment(...)
  function(data, ...) {
    .vars <- rlang::quos(...)
    cols <- names(data)
    if (length(.vars) == 0) {
      selected <- cols
    } else {
      selected <- tidyselect::vars_select(cols, !!!.vars)
    }
    
    grps <- dplyr::groups(data)
    if (length(grps) > 0) {
      group_variables <- selected %in% as.character(grps)
      selected <- selected[!group_variables]
    }
    
    variables <- tibble::tibble(variable = selected)
    nested <- dplyr::mutate(variables,
        skimmed = purrr::map(variable, skim_one, data, local_skimmers, append))
    skimmers_used <- purrr::map(nested$skimmed,
                                ~list(type = attr(.x, "skimmer_type"),
                                      used = attr(.x, "skimmers_used")))
    unique_skimmers <- unique(skimmers_used)
    skimmers <- purrr::map(unique_skimmers, "used")
    variable_types <- purrr::map(unique_skimmers, "type")
    out <- tidyr::unnest(nested)
    structure(out,
              class = c("skim_df", "tbl_df", "tbl", "data.frame"),
              data_rows = nrow(data),
              data_cols = ncol(data),
              df_name = rlang::expr_label(substitute(data)),
              groups = attr(data, "vars"),
              skimmers_used = purrr::set_names(skimmers, variable_types))
  }
}

skim_one <- function(column, data, local_skimmers, append) {
  reduced <- suppressMessages(dplyr::select(data, !!column))
  skimmers <- get_skimmers(reduced[[column]], local_skimmers, append)
  out <- tibble::tibble(type = skimmers$type,
                        !!!dplyr::summarize_all(reduced, skimmers$funs))
  structure(out,
            skimmer_type = skimmers$type,
            skimmers_used = names(skimmers$funs))
}

validate_assignment <- function(...) {
  to_assign <- list(...)
  if (length(to_assign) < 1) return(to_assign)
  
  proposed_names <- names(to_assign)
  if (anyNA(proposed_names)) {
    stop("skim_with requires all arguments to be named.", call. = FALSE)
  }
  
  if ("numeric" %in% proposed_names) {
    warning("Numeric skimming functions are assigned to the `double` type.",
            call. = FALSE)
    to_assign$double <- to_assign$numeric
    to_assign$numeric <- NULL
  }
  
  defaults <- get_default_skimmers()
  existing <- tolower(proposed_names) %in% tolower(proposed_names)
  if (!all(existing)) {
    collapsed <- paste(proposed_names[!existing], collapse = ", ")
    message("Creating new skimming functions for the following classes: ",
            collapsed, "\nThey did not have defaults. Call ",
            "get_default_skimmers() for more infomation.")
  }
  to_assign
}

#' @describeIn skim_with Get a list of classes with default skim functions
#' @export
get_default_skimmers <- function() {
  defaults <- as.character(methods("get_skimmers"))
  classes <- stringr::str_replace(defaults, "get_skimmers.", "")
  purrr::discard(classes, ~.x == "default")
}

#' Create a skimr function list
#' 
#' This is an extension of [dplyr::funs()]. It is used to create a named list
#' of functions. It also you also pass `NULL` to identify a skimming function
#' that you wish to remove.
#' 
#' Only functions that return a single value, working with [dplyr::summarize()],
#' can be used within `sfl`.
#' 
#' @inheritParams dplyr::funs
#' @return A `skimr_function_list`, which contains a list of `fun_calls`,
#'   returned by [dplyr::funs()] and a list of skimming functions to drop.
#' @seealso [dplyr::funs()] and [skim_with()]
#' @export
sfl <- function(..., .args = list()) {
  skimmer_list <- rlang::enquos(...)
  dropable <- purrr::map_lgl(skimmer_list, rlang::quo_is_null)
  keep <- skimmer_list[!dropable]
  drop <- skimmer_list[dropable]
  out <- list(keep = dplyr::funs(!!!keep, .args = .args), drop = names(drop))
  
  structure(out, class = "skimr_function_list")
}

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
#' This function returns the original data. `skim_tee()` uses the default
#' `skim()`, but you can replace it with the `skim` argument.
#' 
#' If you want to work with a data frame that resembles the printed output,
#' call [`skim_to_wide()`] or for a named list of data frames by type
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
#' that are called to produce summary statistics with [`skim_with()`].
#'
#' @section Unicode rendering:
#' If the rendered examples show unencoded values such as `<U+2587>` you will
#' need to change your locale to allow proper rendering. Please review the
#' *Using Skimr* vignette for more information
#' (`vignette("Using_skimr" package = "skimr")`).
#'
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param ...  Columns to select for skimming. When none are provided, the
#'   default is to skim all columns.
#' @param skim  The skimming function to use in `skim_tee()`.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. The result is usually a data frame or tibble.
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
#' # Which column as the greatest mean value?
#' iris %>%
#'   skim() %>%
#'   dplyr::arrange(-mean)
#' 
#' # Use skim_tee to view the skim results and
#' # continue using the original data.
#' chickwts %>%
#'   skim_tee() %>%
#'   dplyr::filter(feed == "sunflower")
#' @export
skim <- skim_with()

#' @rdname skim 
#' @export
skim_tee <- function(.data, ..., skim = skim) {
  print(skim(.data))
  invisible(.data)
}
