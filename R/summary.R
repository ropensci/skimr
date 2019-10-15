#' Summary function for skim_df
#'
#' This is a method of the generic function [`summary()`].
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
  df_name <- attr(object, "df_name")
  df_name <- ifelse(df_name %in% c("`.`", ".data"), "Piped data", df_name)
  df_name <- gsub("`", "", df_name)
  df_name <- ifelse(nchar(df_name) > 25,
    paste0(substring(df_name, 1, 25), "..."),
    df_name
  )

  duplicated <- duplicated(object$skim_variable)
  counts <- table(type = object$skim_type[!duplicated])
  types <- dimnames(counts)[[1]]
  types <- paste0("  ", types)
  possible_groups <- ifelse(is.null(attr(object, "groups")),
    "None",
    paste(as.character(attr(object, "groups")),
      collapse = ", "
    )
  )

  summary_object <- c(
    df_name,
    attr(object, "data_rows"),
    attr(object, "data_cols"),
    " ",
    " ",
    unname(counts),
    "  ",
    possible_groups
  )

  summary_object <- array(summary_object, dim = c(length(summary_object), 1))
  dnames <- c(
    "Name", "Number of rows ", "Number of columns ", "_______________________ ",
    "Column type frequency: ", types, "________________________  ",
    "Group variables"
  )

  summary_object <- as.table(as.array(summary_object))
  dimnames(summary_object) <- list(dnames, c("Values"))
  class(summary_object) <- c("summary_skim_df", "table")
  summary_object
}
