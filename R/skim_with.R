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
#' When `append = TRUE` and local skimmers have names matching the names of
#' entries in the default `skim_function_list`, the values in the default list
#' are overwritten. Similarly, if `NULL` values are passed within `sfl()`, these
#' default skimmers are dropped. Otherwise, if `append = FALSE`, only the
#' locally-provided skimming functions are used.
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
#' my_skim <- skim_with(numeric = sfl(mean), character = sfl(length))
#' 
#' # Or pass the same as a list
#' my_skimmers <- list(numeric = sfl(mean), character = sfl(length))
#' my_skim <- skim_with(!!!my_skimmers)
#' @export
skim_with <- function(..., append = TRUE) {
  local_skimmers <- validate_assignment(...)
  function(data, ...) {
    stopifnot(is.data.frame(data))
    
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

#' Process arguments provided in `skim_with`
#' 
#' Make sure that arguments provided in `skim_with()` have names. Also, 
#' check if we are defining a new skimming type dynamically.
#'
#' @keywords internal
#' @noRd
validate_assignment <- function(...) {
  to_assign <- list(...)
  if (length(to_assign) < 1) return(to_assign)
  
  proposed_names <- names(to_assign)
  if (!all(nzchar(proposed_names)) || is.null(proposed_names) ||
      anyNA(proposed_names)) {
    stop("skim_with requires all arguments to be named.", call. = FALSE)
  }
  
  defaults <- get_default_skimmers()
  existing <- proposed_names %in% names(defaults)
  if (!all(existing)) {
    collapsed <- paste(proposed_names[!existing], collapse = ", ")
    message("Creating new skimming functions for the following classes: ",
            collapsed, ".\nThey did not have recognized defaults. Call ",
            "get_default_skimmers() for more information.")
  }
  to_assign
}

#' Generate one or more rows of a `skim_df`, using one column
#' 
#' Get the default skimmers for the current column using S3 dispatch for
#' [get_skimmers()]. Get the user-provided local skimmers from [skim_with()].
#' If no local skimmers are provided, use the defaults. Otherwise, merge the
#' local and default skimmers with the following rules.
#' 
#'   - If `append = FALSE` of if the local and default types differ, use only
#'     the locals.
#'   - Else, replace the default values with the local values. 
#'
#' Call all of the skimming functions on the single column, using grouped
#' variants, if necessary.
#'
#' @keywords internal
#' @noRd
skim_one <- function(column, data, local_skimmers, append) {
  defaults <- get_skimmers(data[[column]])
  all_classes <- class(data[[column]])
  locals <- get_local_skimmers(all_classes, local_skimmers)
  
  if (is.null(defaults$type)) {
    msg <- sprintf("Default skimming functions for column [%s] with class [%s]",
                   column, paste(all_classes, collapse = ", "))
    stop(msg, " did not have value for its `.class` argument. Please ",
         "investigate the definition of the associated S3 method.",
         call. = FALSE)
  }
  
  if (is.null(locals$keep)) {
    if (defaults$type == "default") {
      warning("Couldn't find skimmers for class: %s; No user-defined `sfl` ",
              "provided. Falling back to `character`.", call. = FALSE)
      data[[column]] <- as.character(data[[column]])
      skimmers <- defaults
      skimmers$type <- "character"
    } else {
      skimmers <- defaults
    }
  } else {
    skimmers <- merge_skimmers(locals, defaults, append)
  }
  
  reduced <- suppressMessages(dplyr::select(data, !!column))
  out <- tibble::tibble(type = skimmers$type,
                        !!!dplyr::summarize_all(reduced, skimmers$keep))
  used <- names(skimmers$keep)
  grps <- dplyr::groups(reduced)
  names(out) <- c("type", as.character(grps), used)
  structure(out,
            skimmer_type = skimmers$type,
            skimmers_used = used)
}

get_local_skimmers <- function(classes, local_skimmers) {
  all_matches <- local_skimmers[classes]
  safe_modify <- purrr::possibly(purrr::list_modify, NULL)
  add_types <- purrr::map2(all_matches, classes, ~safe_modify(.x, type = .y))
  purrr::detect(add_types, ~!is.null(.x))
}

merge_skimmers <- function(locals, defaults, append) {
  if (locals$type != defaults$type || !append) {
    locals
  } else {
    defaults$keep <- purrr::list_modify(defaults$keep, !!!locals$keep)
    defaults$keep[locals$drop] <- NULL
    defaults
  }
}
