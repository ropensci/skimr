#' Set or add the summary functions for a particular type of data
#'
#' While skim is designed around having an opinionated set of defaults, you
#' can use this function to change the summary statistics that it returns.
#'
#' `skim_with()` is a closure: a function that returns a new function. This
#' lets you have several skimming functions in a single R session, but it
#' also means that you need to assign the return of `skim_with()` before
#' you can use it.
#'
#' You assign values within `skim_with` by using the [sfl()] helper (`skimr`
#' function list). This helper behaves mostly like [dplyr::funs()], but lets
#' you also identify which skimming functions you want to remove, by setting
#' them to `NULL`. Assign an `sfl` to each column type that you wish to modify.
#'
#' Functions that summarize all data types, and always return the same type
#' of value, can be assigned to the `base` argument. The default base skimmers
#' compute the number of missing values [n_missing()] and the rate of values
#' being complete, i.e. not missing, [complete_rate()].
#'
#' When `append = TRUE` and local skimmers have names matching the names of
#' entries in the default `skim_function_list`, the values in the default list
#' are overwritten. Similarly, if `NULL` values are passed within `sfl()`, these
#' default skimmers are dropped. Otherwise, if `append = FALSE`, only the
#' locally-provided skimming functions are used.
#'
#' Note that `append` only applies to the `typed` skimmers (i.e. non-base).
#' See [get_default_skimmer_names()] for a list of defaults.
#'
#' @param ... One or more (`sfl`) `skimmer_function_list` objects, with an
#'   argument name that matches a particular data type.
#' @param base An `sfl` that sets skimmers for all column types.
#' @param append Whether the provided options should be in addition to the
#'   defaults already in `skim`. Default is `TRUE`.
#' @return A new `skim()` function. This is callable. See [skim()] for more
#'   details.
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
#'
#' # Use the v1 base skimmers instead.
#' my_skim <- skim_with(base = sfl(
#'   missing = n_missing,
#'   complete = n_complete,
#'   n = length
#' ))
#'
#' # Remove the base skimmers entirely
#' my_skim <- skim_with(base = NULL)
#' @export
skim_with <- function(...,
                      base = sfl(
                        n_missing = n_missing,
                        complete_rate = complete_rate
                      ),
                      append = TRUE) {
  stopifnot(is.null(base) || inherits(base, "skimr_function_list"))
  local_skimmers <- validate_assignment(...)

  function(data, ..., .data_name = NULL) {
    if (is.null(.data_name)) {
      .data_name <- rlang::expr_label(substitute(data))
    }

    if (!inherits(data, "data.frame")) {
      data <- as.data.frame(data)
    }
    stopifnot(inherits(data, "data.frame"))

    selected <- names(tidyselect::eval_select(rlang::expr(c(...)), data))
    if (length(selected) == 0) {
      selected <- names(data)
    }

    grps <- dplyr::groups(data)
    if (length(grps) > 0) {
      group_variables <- selected %in% as.character(grps)
      selected <- selected[!group_variables]
    } else {
      attr(data, "groups") <- list()
    }


    skimmers <- purrr::map(
      selected, get_final_skimmers, data, local_skimmers, append
    )
    types <- purrr::map_chr(skimmers, "skim_type")
    unique_skimmers <- reduce_skimmers(skimmers, types)
    combined_skimmers <- purrr::map(unique_skimmers, join_with_base, base)
    ready_to_skim <- tibble::tibble(
      skim_type = unique(types),
      skimmers = purrr::map(combined_skimmers, mangle_names, names(base$funs)),
      skim_variable = split(selected, types)[unique(types)]
    )
    grouped <- dplyr::group_by(ready_to_skim, .data$skim_type)
    nested <- dplyr::summarize(
      grouped,
      skimmed = purrr::map2(
        .data$skimmers, .data$skim_variable, skim_by_type, data
      )
    )
    structure(
      tidyr::unnest(nested, .data$skimmed),
      class = c("skim_df", "tbl_df", "tbl", "data.frame"),
      data_rows = nrow(data),
      data_cols = ncol(data),
      df_name = .data_name,
      dt_key  = get_dt_key(data),
      groups = dplyr::groups(data),
      base_skimmers = names(base$funs),
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

  if (length(to_assign) < 1) {
    return(to_assign)
  }

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

  if (!all(existing) & length(defaults) > 0) {
    collapsed <- paste(proposed_names[!existing], collapse = ", ")
    message(
      "Creating new skimming functions for the following classes: ",
      collapsed, ".\nThey did not have recognized defaults. Call ",
      "get_default_skimmers() for more information."
    )
  }
  to_assign
}

#' Sets the appropriate key value when working with `data.table`
#' @keywords internal
#' @noRd
get_dt_key <- function(data) {
  if (inherits(data, "data.table")) {
    dt_key <- data.table::key(data)
    if (is.null(dt_key)) {
      dt_key <- "NULL"
    }
    paste(dt_key, collapse = ", ")
  } else {
    NA # Will never be NA if `data` is a data.table
  }
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
#' @param append Same as above.
#' @param base Same as above.
#' @noRd
get_final_skimmers <- function(column, data, local_skimmers, append) {
  defaults <- get_skimmers(data[[column]])
  all_classes <- skim_class(data[[column]])
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
      msg <- sprintf(
        "Couldn't find skimmers for class: %s;",
        paste(all_classes, collapse = ", ")
      )
      warning(msg,
        " No user-defined `sfl` provided. Falling back to `character`.",
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

skim_class <- function(column) {
  base_class <- class(column)
  if (any(base_class %in% c("double", "integer"))) {
    c(base_class, "numeric")
  } else {
    base_class
  }
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
  named <- rlang::set_names(skimmers, types)
  named[unique(types)]
}

join_with_base <- function(skimmers, base) {
  skimmers$funs <- c(base$funs, skimmers$funs)
  skimmers
}

get_skimmers_used <- function(skimmers) {
  types <- names(skimmers)
  function_names <- purrr::map(skimmers, ~ names(.x$funs))
  rlang::set_names(function_names, types)
}

NAME_DELIMETER <- "~!@#$%^&*()-+"
mangle_names <- function(skimmers, base_names) {
  fun_names <- names(skimmers$funs)
  prefixes <- ifelse(
    fun_names %in% base_names,
    NAME_DELIMETER,
    paste0(NAME_DELIMETER, skimmers$skim_type, ".")
  )
  mangled <- paste0(prefixes, fun_names)
  skimmers$funs <- rlang::set_names(skimmers$funs, mangled)
  skimmers
}


#' Generate one or more rows of a `skim_df`, using one column
#'
#' Call all of the skimming functions on the single column, using grouped
#' variants, if necessary.
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
#' with the column name + `_` + our internal delimiter.
#'
#' @param mangled_skimmers The `sfl`'s whose function names have been mangled.
#' @param variable_names The names of columns in the original data, matching a
#'   data type, that will be summarized.
#' @param data The original data.
#' @keywords internal
#' @noRd
skim_by_type <- function(mangled_skimmers, variable_names, data) {
  UseMethod("skim_by_type", data)
}

#' @export
skim_by_type.grouped_df <- function(mangled_skimmers, variable_names, data) {
  group_columns <- dplyr::groups(data)
  grouped <- dplyr::group_by(data, !!!group_columns)
  skimmed <- dplyr::summarize(
    grouped,
    dplyr::across(variable_names, mangled_skimmers$funs)
  )
  build_results(skimmed, variable_names, group_columns)
}

#' @export
skim_by_type.data.frame <- function(mangled_skimmers, variable_names, data) {
  skimmed <- dplyr::summarize(
    data,
    dplyr::across(variable_names, mangled_skimmers$funs)
  )
  build_results(skimmed, variable_names, NULL)
}

#' @export
skim_by_type.data.table <- function(mangled_skimmers, variable_names, data) {
  data <- tibble::as_tibble(data)
  skimmed <- dplyr::summarize(
    data,
    dplyr::across(variable_names, mangled_skimmers$funs)
  )
  build_results(skimmed, variable_names, NULL)
}

#' Summarize returns a single row data frame, make it tall.
#' @noRd
build_results <- function(skimmed, variable_names, groups) {
  if (length(variable_names) > 1) {
    out <- tibble::tibble(
      skim_variable = variable_names,
      by_variable = purrr::map(variable_names, reshape_skimmed, skimmed, groups)
    )
    tidyr::unnest(out, .data$by_variable)
  } else {
    out <- dplyr::select(
      as.data.frame(skimmed),
      !!!groups,
      tidyselect::contains(NAME_DELIMETER)
    )
    tibble::tibble(
      skim_variable = variable_names,
      !!!set_clean_names(out)
    )
  }
}

reshape_skimmed <- function(column, skimmed, groups) {
  delim_name <- paste0(column, "_", NAME_DELIMETER)
  out <- dplyr::select(
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    tibble::as_tibble(skimmed),
=======
    as_tibble(skimmed),
>>>>>>> ded9895 (relace-as.data.frame)
=======
    as_tibble(skimmed),
>>>>>>> 406674d (relace-as.data.frame)
=======
    tibble::as_tibble(skimmed),
>>>>>>> e27fd8d (fn)
=======
    as_tibble(skimmed),
>>>>>>> ded9895 (relace-as.data.frame)
    !!!groups,
    tidyselect::starts_with(delim_name, ignore.case = FALSE)
  )
  set_clean_names(out)
}

set_clean_names <- function(out) {
  separated <- strsplit(names(out), NAME_DELIMETER, fixed = TRUE)
  clean_names <- purrr::map_chr(separated, ~ .x[length(.x)])
  rlang::set_names(out, clean_names)
}
