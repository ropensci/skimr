#' Print `skim` objects
#'
#' `skimr` has custom print methods for all supported objects. Default printing
#' methods for `knitr`/ `rmarkdown` documents is also provided.
#'
#' @section Printing options:
#'
#' For better or for worse, `skimr` often produces more output than can fit in
#' the standard R console. Fortunately, most modern environments like RStudio
#' and Jupyter support more than 80 character outputs. Call
#' `options(width = 90)` to get a better experience with `skimr`.
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
  cat(paste0("\n",top_line), out[-metadata], sep = "\n")
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
  print.table(x)
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
#' @param options Options passed into the print function.
#' @param ... Additional arguments passed to method
#' @export
knit_print.skim_df <- function(x, options = NULL, ...) {
  if (is_skim_df(x)) {
    if (options$skimr_include_summary %||% TRUE) {
      summary_stats <- summary(x)

      kabled <- knitr::kable(
        summary_stats,
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
  } else {
    NextMethod("knit_print")
  }
}

knit_print_by_type <- function(x, options, summary) {
  all_tables <- purrr::imap(x, knit_print_one, options)
  combined <- c("", summary, "", "", unlist(all_tables))
  knitr::asis_output(paste(combined, collapse = "\n"))
}

knit_print_one <- function(by_type, type, options) {
  kabled <- knitr::kable(
    by_type, digits = options$skimr_digits %||% 2
  )
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

  kabled <- knitr::kable(
    x,
    table.attr = "style='width: auto;'
      class='table table-condensed'",
    col.names = c(" "),
    caption = "Data summary"
  )

  knitr::asis_output(paste(kabled, collapse = "\n"))
}


#' Skimr printing within Jupyter notebooks
#' 
#' This reproduces printed results in the console. By default Jupyter kernels
#' render the final object in the cell. We want the version printed by
#' `skimr` instead of the data that it contains.
#' 
#' @param obj The object to \link{print} and then return the output.
#' @param ... ignored.
#' @return None. `invisible(NULL)`.
#' @importFrom repr repr_text
#' @name repr

#' @rdname repr
#' @export
repr_text.skim_df <- function(obj, ...) {
  print(obj)
}

#' @rdname repr
#' @export
repr_text.skim_list <- repr_text.skim_df

#' @rdname repr
#' @export
repr_text.one_skim_df <- repr_text.skim_df
