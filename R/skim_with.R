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
#' # For example, you might want to remove NA values.
#' my_skim <- skim_with(numeric = sfl(iqr = ~ IQR(., na.rm = TRUE)))
#'
#' # Set multiple types of skimmers simultaneously.
#' my_skim <- skim_with(numeric = sfl(mean), character = sfl(length))
#'
#' # Or pass the same as a list, unquoting the input.
#' my_skimmers <- list(numeric = sfl(mean), character = sfl(length))
#' my_skim <- skim_with(!!!my_skimmers)
#' @export
skim_with <- function(..., append = TRUE) {
  local_skimmers <- validate_assignment(...)

  function(data, ...) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
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

    skimmers <- purrr::map(
      selected, get_final_skimmers, data, local_skimmers, append
    )
    types <- purrr::map_chr(skimmers, "skim_type")
    unique_skimmers <- reduce_skimmers(skimmers, types)
    ready_to_skim <- tibble::tibble(
      skim_type = unique(types),
      skimmers = unique_skimmers,
      skim_variable = split(selected, types)[!!rlang::sym("skim_type")]
    )
    grouped <- dplyr::group_by(ready_to_skim, !!rlang::sym("skim_type"))
    nested <- dplyr::summarize(
      grouped,
      skimmed = purrr::map2(
        !!rlang::sym("skimmers"),
        !!rlang::sym("skim_variable"),
        skim_by_type,
        data
      )
    )
    structure(tidyr::unnest(nested),
      class = c("skim_df", "tbl_df", "tbl", "data.frame"),
      data_rows = nrow(data),
      data_cols = ncol(data),
      df_name = rlang::expr_label(substitute(data)),
      groups = dplyr::groups(data),
      skimmers_used = get_skimmers_used(unique_skimmers)
    )
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
  to_assign <- rlang::list2(...)

  if (length(to_assign) < 1) return(to_assign)

  # Need to cope with case where ... is a list already
  if (class(to_assign[[1]]) != "skimr_function_list") {
    to_assign <- to_assign[[1]]
  }

  proposed_names <- names(to_assign)
  if (!all(nzchar(proposed_names)) || is.null(proposed_names) ||
    anyNA(proposed_names)) {
    stop("skim_with requires all arguments to be named.", call. = FALSE)
  }

  defaults <- get_default_skimmers()
  existing <- proposed_names %in% names(defaults)
  if (!all(existing)) {
    collapsed <- paste(proposed_names[!existing], collapse = ", ")
    message(
      "Creating new skimming functions for the following classes: ",
      collapsed, ".\nThey did not have recognized defaults. Call ",
      "get_default_skimmers() for more information."
    )
  }
  to_assign
}

#' Combine local and default skimmers for each column
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
#' @param column A character scalar. The column name.
#' @param data The data frame to summarize.
#' @param local_skimmers A list of `sfl` objects. Skimmers defined using
#'   `skim_with()`
#' @noRd
get_final_skimmers <- function(column, data, local_skimmers, append) {
  defaults <- get_skimmers(data[[column]])
  all_classes <- class(data[[column]])
  locals <- get_local_skimmers(all_classes, local_skimmers)

  if (!nzchar(defaults$skim_type)) {
    msg <- sprintf(
      "Default skimming functions for column [%s] with class [%s]",
      column, paste(all_classes, collapse = ", ")
    )
    stop(msg, " did not have value for its `.class` argument. Please ",
      "investigate the definition of the associated S3 method.",
      call. = FALSE
    )
  }

  if (is.null(locals$funs)) {
    if (defaults$skim_type == "default") {
      warning("Couldn't find skimmers for class: %s; No user-defined `sfl` ",
        "provided. Falling back to `character`.",
        call. = FALSE
      )
      data[[column]] <- as.character(data[[column]])
      skimmers <- defaults
      skimmers$skim_type <- "character"
    } else {
      skimmers <- defaults
    }
  } else {
    skimmers <- merge_skimmers(locals, defaults, append)
  }
  skimmers
}

get_local_skimmers <- function(classes, local_skimmers) {
  all_matches <- local_skimmers[classes]
  safe_modify <- purrr::possibly(purrr::list_modify, NULL)
  add_types <- purrr::map2(
    all_matches,
    classes,
    ~ safe_modify(.x, skim_type = .y)
  )
  purrr::detect(add_types, ~ !is.null(.x))
}

merge_skimmers <- function(locals, defaults, append) {
  if (!append || locals$skim_type != defaults$skim_type) {
    locals
  } else {
    defaults$funs <- purrr::list_modify(defaults$funs, !!!locals$funs)
    defaults
  }
}

reduce_skimmers <- function(skimmers, types) {
  named <- purrr::set_names(skimmers, types)
  named[unique(types)]
}

get_skimmers_used <- function(skimmers) {
  types <- names(skimmers)
  function_names <- purrr::map(skimmers, ~ names(.x$funs))
  purrr::set_names(function_names, types)
}

#' Generate one or more rows of a `skim_df`, using one column
#'
#' Call all of the skimming functions on the single column, using grouped
#' variants, if necessary.
#'
#' @keywords internal
#' @noRd
skim_by_type <- function(skimmers, data_columns, data) {
  UseMethod("skim_by_type", data)
}

#' @export
skim_by_type.grouped_df <- function(skimmers, data_columns, data) {
  group_columns <- dplyr::groups(data)
  new_names <- names(skimmers$funs)
  delim <- "~!@#$%^&*()-+"
  mangled_skimmers <- purrr::set_names(skimmers$funs, paste0(delim, new_names))
  grouped <- dplyr::group_by(data, !!!group_columns)
  skimmed <- dplyr::summarize_at(grouped, data_columns, mangled_skimmers)
  build_results(skimmed, data, data_columns, group_columns, new_names, delim)
}

#' @export
skim_by_type.default <- function(skimmers, data_columns, data) {
  new_names <- names(skimmers$funs)
  delim <- "~!@#$%^&*()-+"
  mangled_skimmers <- purrr::set_names(skimmers$funs, paste0(delim, new_names))
  skimmed <- dplyr::summarize_at(data, data_columns, mangled_skimmers)
  build_results(skimmed, data, data_columns, NULL, new_names, delim)
}

#' Summarize returns a single row data frame, make it tall.
#'
#' We expect one row per variable/ group. To do this we need to take the
#' processed results, find the appropriate columns for each variable and
#' restack them. This uses a small hack that rests on the naming convention
#' of data frame produced by `summarize_at`, which uses the following scheme:
#'
#'  - `variable_name` + `_` + `function_name`
#'
#' To avoid inappropriately assigning the columns to the wrong variable, we
#' mangle the function names. That way, each set of relevant columns begin
#' with the column name + `_` + our internal delimeter.
#' @noRd
build_results <- function(skimmed, data, data_cols, groups, new_names, delim) {
  if (length(data_cols) > 1) {
    out <- tibble::tibble(
      skim_variable = data_cols,
      by_variable = purrr::map(
        data_cols, reshape_skimmed, skimmed, groups, new_names, delim
      )
    )
    tidyr::unnest(out)
  } else {
    new_names <- c(groups, new_names)
    tibble::tibble(
      skim_variable = data_cols,
      !!!purrr::set_names(skimmed, new_names)
    )
  }
}

reshape_skimmed <- function(column, skimmed, groups, new_names, delim) {
  delim_name <- paste0(column, "_", delim)
  out <- dplyr::select(
    skimmed,
    !!!groups,
    tidyselect::starts_with(delim_name)
  )
  purrr::set_names(out, c(groups, new_names))
}
