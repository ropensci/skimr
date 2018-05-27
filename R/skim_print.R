#' Print `skim` objects
#' 
#' `skimr` has custom print methods for all supported objects. Default printing
#' methods for `knitr`/ `rmarkdown` documents is also provided.
#' 
#' @param x Either a `skim_df`, `skim_vector` or `skim_summary` object.
#' @param ... Further arguments passed to or from other methods.
#' @name print
NULL

#' @describeIn print Print a skimmed data frame (`skim_df` from [`skim()`]).
#' @export
print.skim_df <- function(x, include_summary = TRUE, ...) {
  if (include_summary) {
    cat("Skim summary statistics\n")
    cat(" n obs:", attr(x, "data_rows"), "\n")
    cat(" n variables:", attr(x, "data_cols"), "\n")
  }

  by_type <- partition(x)
  purrr::imap(by_type, print)
  invisible(NULL)
}

#' @describeIn print Print an entry within a partitioned `skim_df`.
#' @export
print.one_skim_df <- function(x, ..) {
  variable_type <- paste("Variable type:", attr(x, "type"))
  with_line <- cli::rule(line = 1, left = variable_type)
  print(with_line)
  NextMethod("print")
}

#' @describeIn print Print a `skim_list`, a list of `skim_df` objects.
#' @export
print.skim_list <- function(x, ..) {
  nms <- names(x)
  attributes(x) <- NULL
  print(purrr::set_names(x, nms))
}


#' @describeIn print Print method for a `summary_skim_df` object.
#' @export
print.summary_skim_df <- function(x, ...) {
  n_rows <- paste0("Number of Rows: ", x$n_rows, "   \n")
  n_cols <- paste0("Number of Columns: ", x$n_cols, "    \n")
  df_name <- ifelse(x$df_name == ".", "", paste0("Name: ", x$df_name, "   \n"))
  
  type_frequency_string <- paste0(x$type_frequencies$type,
                                  ": ",
                                  x$type_frequencies$n, 
                                  collapse = "   \n")

  cat("A skim object    \n\n",
      df_name,
      n_rows, 
      n_cols, 
      "    \nColumn type frequency    \n",
      type_frequency_string,
      "\n"
      ,sep = "")
}

#' Provide a default printing method for knitr.
#' 
#' Instead of standard R output, `knitr` and `RMarkdown` documents will have
#' formatted [knitr::kable()] output on return. You can disable this by setting
#' the chunk option `render = normal_print`.
#' 
#' The summary statistics for the original data frame can be disabled by setting
#' the `knitr` chunk option `skimr_include_summary = FALSE`. See
#' [knitr::opts_chunk] for more information. You can change the number of digits
#' shown in the printed table with the `skimr_digits` chunk option.
#' 
#' Alternatively, you can call [collapse()] or [yank()] to get the particular
#' `skim_df` objects and format them however you like. One warning though.
#' Because histograms contain unicode characters, they can have unexpected
#' print results, as R as varying levels of unicode support. This affects
#' Windows users most commonly. Call `vignette("Using_fonts")` for more details.
#'
#' @seealso [knitr::kable()]
#' @inheritParams knitr::knit_print
#' @return A `knit_asis` object. Which is used by `knitr` when renderind.
#' @importFrom knitr knit_print
#' @name knit_print
NULL

#' @describeIn knit_print Default `knitr` print for `skim_df` objects.
#' @export
knit_print.skim_df <- function(x, options, ...) {
  if (options$skimr_include_summary %||% TRUE) {
    summary_stats <- data.frame(
      n_obs = attr(x, "data_rows"),
      n_cols =attr(x, "data_cols"))
    kabled <- knitr::kable(
      summary_stats, format = "html", 
      table.attr = "style='width: auto;' class='table table-condensed'")
    summary <- c("**Skim summary statistics**", "", kabled, "", "")
  } else {
    summary <- c()
  }
  by_type <- partition(x)
  knit_print_by_type(by_type, options, summary)
}

knit_print_by_type <- function(x, options, summary) {
  all_tables <- purrr::imap(x, knit_print_one, options)
  combined <- c("", summary, "", "", unlist(all_tables))
  knitr::asis_output(paste(combined, collapse = "\n"))
}

knit_print_one <- function(by_type, skim_type, options) {
  kabled <- knitr::kable(by_type, digits = options$skimr_digits %||% 2)
  if (is_windows()) {
    kabled[] <- fix_unicode(kabled)
  }
  caption <- sprintf("**Variable type: %s**", skim_type)
  c(caption, "", kabled, "", "")
}

#' @describeIn print Default `knitr` print for a `skim_list`.
#' @export
knit_print.skim_list <- function(x, options, ...) {
  knit_print_by_type(x, options, NULL)
}

#' @describeIn print Default `knitr` print within a partitioned `skim_df`.
#' @export
knit_print.one_skim_df <- function(x, options, ...) {
  kabled <- knit_print_one(x, attr(x, "type"), options)
  combined <- c("", "", kabled, "")
  knitr::asis_output(paste(combined, collapse = "\n"))
}

#' @describeIn knit_print Default `knitr` print for `skim_df` summaries.
#' @export
knit_print.summary_skim_df <- function(x, options, ...) {
  knit_print_one()
  n_rows <- paste0("Number of Rows: ", x[["n_rows"]])
  n_cols <- paste0("Number of Columns: ", x[["n_cols"]])
  df_name <- paste0("Name: ", x[["df_name"]])
  
  kframe <- data.frame(df_name, n_rows, n_cols)
  list(Summary = kable(kframe),
       `Type counts` = kable(x$type_frequencies))
}
