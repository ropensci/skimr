#' Summary function for skim_df
#' 
#' This is a method of the generic function [`summary()`].
#' 
#' @param object a skim dataframe.
#' @param ... Additional arguments affecting the summary produced. Not used.
#' @return A summary of the skim data frame.
#' @examples
#' \dontrun{
#'  a <- skim(mtcars)
#'  summary(a)
#' }
#' @export
summary.skim_df <- function(object, ...){
  if (is.null(object)) {
    stop("dataframe is null.")
  }

  duplicated <- duplicated(object$variable)
  counts <- table(type = object$type[!duplicated])
  type_frequencies <- tibble::as_tibble(counts)

  summary_object <- list(
    df_name = attr(object, "df_name"),
    n_rows = attr(object, "data_rows"),
    n_cols = attr(object, "data_cols"),
    type_frequencies = type_frequencies
  )

  structure(summary_object, class = c("summary_skim_df", "list"))
}
