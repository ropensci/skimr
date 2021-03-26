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
  data_name <- df_name(object)
  data_name <- ifelse(data_name %in% c("`.`", ".data"), "Piped data", data_name)
  data_name <- gsub("`", "", data_name)
  data_name <- ifelse(nchar(data_name) > 25,
    paste0(substring(data_name, 1, 25), "..."),
    data_name
  )

  duplicated <- duplicated(object$skim_variable)
  counts <- table(type = object$skim_type[!duplicated])
  types <- dimnames(counts)[[1]]
  types <- paste0("  ", types)
  possible_names <- group_names(object)
  possible_groups <- if (length(possible_names) > 0) {
    paste(possible_names, collapse = ", ")
  } else {
    "None"
  }

  summary_object <- c(
    data_name,
    data_rows(object),
    data_cols(object),
    if (!is.na(dt_key(object))) dt_key(object),
    " ",
    " ",
    unname(counts),
    " ",
    possible_groups
  )

  summary_object <- array(summary_object, dim = c(length(summary_object), 1))
  dnames <- c(
    "Name", "Number of rows ", "Number of columns ",
    if (!is.na(dt_key(object))) "Key", "_______________________ ",
    "Column type frequency: ", types, "________________________  ",
    "Group variables"
  )

  summary_object <- as.table(summary_object)
  dimnames(summary_object) <- list(dnames, c("Values"))
  class(summary_object) <- c("summary_skim_df", "table")
  summary_object
}
