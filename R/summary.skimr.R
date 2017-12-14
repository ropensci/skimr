#' Summary function for skim_df. This is a method of the generic function \code{summary}
#' 
#' @param object a skim dataframe
#' @param ... Additional arguments affecting the summary produced. Not used
#' @return A summary of the dataframe \code{df}
#' @export
#' @examples
#' 
#'\dontrun{
#' a <- skim(mtcars)
#' summary(a)
#' }
#'
summary.skim_df <- function(object, ...){

  if (is.null(object)) {
    stop("dataframe is null.")
  }
  
  type_frequencies <- object %>%
        dplyr::select(!!rlang::sym("variable"), !!rlang::sym("type")) %>%
        dplyr::distinct() %>%
        dplyr::group_by(!!rlang::sym("type")) %>%
        dplyr::summarise(type_count = n())
  
  summary_object <- list(
    df_name = attr(object, "df_name"),
    n_rows = attr(object, "data_rows"),
    n_cols = attr(object, "data_cols"),
    type_frequencies = type_frequencies
  )
  
  class(summary_object) <- c("summary_skim_df", "list")
  
  summary_object
}



#' Print method for a summary_skim_df object. 
#' This is a method for the generic function \code{print}
#' 
#' @param x a skim_summary object
#' @param ... Additional arguments affecting the print output produced. Not used
#' @export
#' 
print.summary_skim_df <- function(x, ...) {
  
  n_rows <- paste0("Number of Rows: ", x$n_rows, "   \n")
  n_cols <- paste0("Number of Columns: ", x$n_cols, "    \n")
  df_name <- paste0("Name: ", x$df_name, "   \n")
  
  type_frequency_string <- paste0(x$type_frequencies$type,
                                  ": ",
                                  x$type_frequencies$type_count, 
                                  collapse = "   \n")
  
  
  cat("A skim object    \n\n",
      df_name,
      n_rows, 
      n_cols, 
      "    \nColumn type frequency    \n",
      type_frequency_string
      ,sep = "")
  
}

#' Pander method for a summary_skim_df object. 
#' 
#' This is a method for the generic function \code{pander}
#' 
#' @param x a skim_summary object
#' @param ... Additional arguments affecting the print output produced. Not used
#' @export
#' 
pander.summary_skim_df <- function(x, ...) {
  
  n_rows <- paste0("Number of Rows: ", x[["n_rows"]])
  n_cols <- paste0("Number of Columns: ", x[["n_cols"]])
  df_name <- paste0("Name: ", x[["df_name"]])

  plist <- list(df_name, n_rows, n_cols, x$type_frequencies)
  pander(plist)
}

#' Kable method for a summary_skim_df object. 
#' 
#' This is a method for the generic function \code{kable}
#' 
#' @param x a skim_summary object
#' @param ... Additional arguments affecting the print output produced. Not used
#' @export
#' 
kable.summary_skim_df <- function(x, ...) {
  
  n_rows <- paste0("Number of Rows: ", x[["n_rows"]])
  n_cols <- paste0("Number of Columns: ", x[["n_cols"]])
  df_name <- paste0("Name: ", x[["df_name"]])
  
  kframe <- data.frame(df_name, n_rows, n_cols)
  list( Summary = kable(kframe), `Type counts` = kable(x$type_frequencies))
}

