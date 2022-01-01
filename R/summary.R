#' Summary function for skim_df
#'
#' This is a method of the generic function [summary()].
#'
#' @param object a skim dataframe.
#' @param ... Additional arguments affecting the summary produced. Not used.
#' @return A summary of the skim data frame.
#' @examples
#' a <- skim(mtcars)
#' summary(a)
#' @export
summary.skim_df <- function(object, ...) {
  if (is.null(object)) {
    stop("dataframe is null.")
  }
  duplicated <- duplicated(object$skim_variable)
  counts <- table(type = object$skim_type[!duplicated])
  types <- dimnames(counts)[[1]]

  possible_names <- group_names(object)
  possible_groups <- if (length(possible_names) > 0) {
    paste(possible_names, collapse = ", ")
  } else {
    "None"
  }

  structure(
    list(
      data_name = process_data_name(object),
      counts = counts,
      types = types,
      possible_groups = possible_groups,
      dt_key = dt_key(object),
      data_rows = data_rows(object),
      data_cols = data_cols(object)
    ),
    class = "summary_skim_df"
  )
}

process_data_name <- function(object) {
  raw_name <- df_name(object)
  no_ticks <- gsub("`", "", raw_name)
  if (no_ticks %in% c(".", ".data")) {
    "Piped data"
  } else if (nchar(no_ticks) > 25) {
    paste0(substring(no_ticks, 1, 25), "...")
  } else {
    no_ticks
  }
}

#' @export
format.summary_skim_df <- function(x, ...) {
  dnames <- c("", get_summary_dnames(x))
  summary_values <- c("Values", get_summary_values(x))
  paste(
    format(dnames),
    format(summary_values)
  )
}

get_summary_dnames <- function(summary_object) {
  c(
    "Name",
    "Number of rows ",
    "Number of columns ",
    if (!is.na(summary_object$dt_key)) "Key",
    "_______________________ ",
    "Column type frequency: ",
    paste0("  ", summary_object$types),
    "________________________  ",
    "Group variables"
  )
}

get_summary_values <- function(summary_object) {
  c(
    summary_object$data_name,
    summary_object$data_rows,
    summary_object$data_cols,
    if (!is.na(summary_object$dt_key)) summary_object$dt_key,
    " ",
    " ",
    unname(summary_object$counts),
    " ",
    summary_object$possible_groups
  )
}
