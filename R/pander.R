#' Render data frames as markdown
#' @describeIn pander Produce `pander` output
#' @param x Object to be processed
#' @param caption A character scalar specifying the table's caption.
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @seealso [`pander::pander()`]

pander <- function(x, caption = attr(x, "caption"), ...) {
     pander::pander(x, caption = attr(x, "caption"), ...) 
  }

#' @describeIn pander Produce `pander` output of a skimmed data frame
#' @export

pander.skim_df <- function(x, caption = attr(x, "caption"), ...) {
  defaults <- options(dplyr.show_progress = FALSE)
  on.exit(options(defaults))
  
  if (is_windows()) {
    warning("Skimr's histograms incorrectly render with pander on Windows.",
            " Removing them. Use kable() if you'd like them rendered.",
            call. = FALSE)
  }
  cat("Skim summary statistics  \n  ")
  # Spaces are markdown new lines.
  cat(" n obs:", attr(x, "data_rows"), "   \n")
  cat(" n variables:", attr(x, "data_cols"), "   \n")
  
  grps <- dplyr::groups(x) 
  grouped <- dplyr::group_by(x, !!rlang::sym("type"))
  dplyr::do(grouped, skim_render(., grps, pander_impl, caption))
  invisible(x)
}

pander_impl <- function(transformed_df, skim_type, caption) {
  if (is.null(caption)){
    # Intentionally commented due to issue in pandoc
    # caption = cat(sprintf("\nVariable type: %s", skim_type))
  }
  if (is_windows()) {
    transformed_df$hist <- NULL 
  }
  transformed_df <- dplyr::ungroup(transformed_df)
  pander(structure(transformed_df, class = "data.frame"))
  transformed_df
}


#' @describeIn pander Pander method for a summary_skim_df object 
#' @export

pander.summary_skim_df <- function(x, ...) {
  n_rows <- paste0("Number of Rows: ", x[["n_rows"]])
  n_cols <- paste0("Number of Columns: ", x[["n_cols"]])
  df_name <- paste0("Name: ", x[["df_name"]])

  plist <- list(df_name, n_rows, n_cols, x$type_frequencies)
  pander(plist)
}
