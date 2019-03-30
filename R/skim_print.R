#' Print `skim` objects
#'
#' `skimr` has custom print methods for all supported objects. Default printing
#' methods for `knitr`/ `rmarkdown` documents is also provided.
#'
#' @section Behavior in `dplyr` pipelines:
#'
#' Printing a `skim_df` requires specific columns that might be dropped when
#' using [dplyr::select()] or [dplyr::summarize()] on a `skim_df`. In those
#' cases, this method falls back to [tibble::print.tbl()].
#'
#' @inheritParams tibble:::print.tbl
#' @param include_summary Whether a summary of the data frame should be printed
#' @name print
NULL

#' @describeIn print Print a skimmed data frame (`skim_df` from [`skim()`]).
#' @export
print.skim_df <- function(x, include_summary = TRUE, n = Inf, width = Inf,
                          n_extra = NULL, ...) {
  if (is_skim_df(x)) {
    if (include_summary) print(summary(x))
    by_type <- partition(x)
    purrr::imap(by_type, print, n, width, n_extra)
    invisible(NULL)
  } else {
    NextMethod("print")
  }
}

#' @describeIn print Print an entry within a partitioned `skim_df`.
#' @export
print.one_skim_df <- function(x, n = Inf, width = Inf, n_extra = NULL, ...) {
  variable_type <- paste("Variable type:", attr(x, "skim_type"))
  top_line <- cli::rule(line = 1, left = variable_type)
  out <- format(x, n = n, width = width, n_extra = n_extra, ...)
  metadata <- grab_tibble_metadata(out)
  render_skim_body(top_line, out, metadata)
}

grab_tibble_metadata <- function(x) {
  if (crayon::has_color()) {
    grep("^\\\033\\[38;5;\\d{3}m[#\\*]", x)
  } else {
    grep("^[#\\*]", x)
  }
}

render_skim_body <- function(top_line, out, metadata) {
  cat(top_line, out[-metadata], sep = "\n")
}

#' @describeIn print Print a `skim_list`, a list of `skim_df` objects.
#' @export
print.skim_list <- function(x, n = Inf, width = Inf, n_extra = NULL, ...) {
  nms <- names(x)
  attributes(x) <- NULL
  print(rlang::set_names(x, nms))
}


#' @describeIn print Print method for a `summary_skim_df` object.
#' @export
print.summary_skim_df <- function(x, ...) {
  cat(paste0(cli::rule(line = 1, left = "Data Summary", width = 40), "\n"))
  print(build_summary_string(x))
}


build_summary_string <- function(x) {
  df_name <- ifelse(x$df_name %in% c("`.`", ".data"), "Piped data", x$df_name)
  df_name <- gsub("`", "", df_name)
  df_name <- ifelse(nchar(df_name) > 25, paste0(substring(df_name, 1, 25), "..."), df_name)
  groups <- ifelse(is.null(x$possible_groups), "None", paste0(x$possible_groups, collapse = ", "))
  types <- paste0("  ", x$type_frequencies$type)
  summary <- data.frame("Value" = c(
    df_name, x$n_rows, x$n_cols, "", "",
    x$type_frequencies$n, "", groups, ""
  ))
  row.names(summary) <- c(
    "Name", "Number of rows ", "Number of columns ", " ",
    "Column type frequency: ", types, "  ",
    "Group variables", "   "
  )
  summary
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
#' @param x A skim_df object.
#' @param options Options passed into the print function
#' @param ... Additional arguments passed to method
#' @export
knit_print.skim_df <- function(x, options = NULL, ...) {
  assert_is_skim_df(x)
  if (options$skimr_include_summary %||% TRUE) {
    summary_stats <- summary(x)
    summary_string <- build_summary_string(summary_stats)
    kabled <- knitr::kable(
      summary_string,
      # format = "html",
      table.attr = "style='width: auto;'
      class='table table-condensed'",
      col.names = c(" "),
      caption = "Data summary"
    )
  } else {
    kabled <- c()
  }
  by_type <- partition(x)

  knit_print_by_type(by_type, options, kabled)
}

knit_print_by_type <- function(x, options, summary) {
  all_tables <- purrr::imap(x, knit_print_one, options)
  combined <- c("", summary, "", "", unlist(all_tables))
  knitr::asis_output(paste(combined, collapse = "\n"))
}

knit_print_one <- function(by_type, type, options) {
  kabled <- knitr::kable(by_type, digits = options$skimr_digits %||% 2)
  if (is_windows()) {
    kabled[] <- fix_unicode(kabled)
  }
  caption <- sprintf("**Variable type: %s**", type)
  c(caption, "", kabled, "", "")
}

#' @describeIn knit_print Default `knitr` print for a `skim_list`.
#' @export
knit_print.skim_list <- function(x, options = NULL, ...) {
  knit_print_by_type(x, options, NULL)
}

#' @describeIn knit_print Default `knitr` print within a partitioned `skim_df`.
#' @export
knit_print.one_skim_df <- function(x, options = NULL, ...) {
  kabled <- knit_print_one(x, attr(x, "skim_type"), options)
  combined <- c("", "", kabled, "")
  knitr::asis_output(paste(combined, collapse = "\n"))
}

#' @describeIn knit_print Default `knitr` print for `skim_df` summaries.
#' @export
knit_print.summary_skim_df <- function(x, options = NULL, ...) {
  summary_string <- build_summary_string(x)

  kabled <- knitr::kable(
    summary_string,
    # format = "html",
    table.attr = "style='width: auto;'
      class='table table-condensed'",
    col.names = c(" "),
    caption = "Data summary"
  )

  knitr::asis_output(paste(kabled, collapse = "\n"))
}
