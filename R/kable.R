#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' This is a very simple table generator. It is simple by design. It is not
#' intended to replace any other R packages for making tables.
#' 
#' @seealso [`knitr::kable()`]
#' @inheritParams knitr::kable
#' @return The original `skim_df` object.
#' @export

kable <- function (x, format = NULL, digits = getOption("digits"), 
                   row.names = NA, col.names = NA, align, caption = NULL,
                   format.args = list(), escape = TRUE, ...) {
  UseMethod("kable")
}

#' @export

kable.data.frame <- function(x, format = NULL, digits = getOption("digits"), 
                             row.names = NA, col.names = NA, align = NULL, 
                             caption = NULL, format.args = list(), 
                             escape = TRUE, ...) { 
       knitr::kable(x, format = NULL, digits = getOption("digits"), 
                    row.names = NA, col.names = NA, align = NULL, 
                    caption = NULL, format.args = list(), 
                    escape = TRUE, ...) 
  }

#' @describeIn kable Produce `kable` output of a skimmed data frame
#' @export

kable.skim_df <- function(x, format = NULL, digits = getOption("digits"), 
                          row.names = NA, col.names = NA, align = NULL, 
                          caption = NULL, format.args = list(), 
                          escape = TRUE, ...) {
  defaults <- options(dplyr.show_progress = FALSE)
  on.exit(options(defaults))
  
  # Spaces are markdown new lines
  cat("Skim summary statistics  \n")
  cat(" n obs:", attr(x, "data_rows"), "   \n")
  cat(" n variables:", attr(x, "data_cols"), "   \n")
  grps <- dplyr::groups(x) 
  grouped <- dplyr::group_by(x, !!rlang::sym("type"))
  dplyr::do(grouped, skim_render(., grps, kable_impl, format, digits, 
                                 row.names, col.names, align, caption, 
                                 format.args, escape, ...))
  invisible(x)
}

kable_impl <- function(transformed_df, skim_type, format , digits, row.names, 
                       col.names, align, caption, format.args, 
                       escape, ...) {
  cat(sprintf("\nVariable type: %s", skim_type))
  if(is.null(align)) align <- rep("l", length(transformed_df))
  kabled <- kable(transformed_df, caption = NULL, align = align, format, digits,
        row.names,  col.names, format.args,  escape, ...)
  if (is_windows()) {
    kabled[] <- fix_unicode(kabled)
  }
  print(kabled)
  transformed_df
}


#' @describeIn kable Kable method for a `summary_skim_df` object
#' @export

kable.summary_skim_df <- function(x, ...) {
  n_rows <- paste0("Number of Rows: ", x[["n_rows"]])
  n_cols <- paste0("Number of Columns: ", x[["n_cols"]])
  df_name <- paste0("Name: ", x[["df_name"]])
  
  kframe <- data.frame(df_name, n_rows, n_cols)
  list( Summary = kable(kframe), `Type counts` = kable(x$type_frequencies))
}
