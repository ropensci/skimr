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
#' The print methods in `skimr` wrap those in the [tibble][tibble::formatting]
#' package. You can control printing behavior using the same global options.
#'
#' @section Behavior in `dplyr` pipelines:
#'
#' Printing a `skim_df` requires specific columns that might be dropped when
#' using [dplyr::select()] or [dplyr::summarize()] on a `skim_df`. In those
#' cases, this method falls back to [tibble::print.tbl()].
#'
#' @section Controlling metadata behavior:
#'
#' On POSIX systems, `skimr` removes the tibble metadata when generating output.
#' On some platforms, this can lead to all output getting removed. To disable
#' that behavior, set either `strip_metadata = FALSE` when calling print or use
#' `options(skimr_strip_metadata = FALSE)`. The `crayon` package and the color
#' support within `tibble` is also a factor. If your `skimr` results tables are
#' empty you may need to run the following `options(crayon.enabled = FALSE)`.
#'
#' @inheritParams tibble:::print.tbl
#' @seealso [tibble::trunc_mat()] For a list of global options for customizing
#'   print formatting. [crayon::has_color()] for the variety of issues that
#'   affect tibble's color support.
#' @param include_summary Whether a summary of the data frame should be printed
#' @param strip_metadata Whether tibble metadata should be removed.
#' @param rule_width  Width of the cli rules in printed skim object. Defaults
#'     to base::options()$width
#' @param summary_rule_width Width of Data Summary cli rule, defaults to 40.
#' @name print
NULL

#' @describeIn print Print a skimmed data frame (`skim_df` from [skim()]).
#' @export
print.skim_df <- function(x,
                          include_summary = TRUE,
                          n = Inf,
                          width = Inf,
                          n_extra = NULL,
                          strip_metadata = getOption(
                            "skimr_strip_metadata", FALSE
                          ),
                          rule_width = base::options()$width,
                          summary_rule_width = 40,
                          ...) {
  withr::local_options(list(crayon.enabled = FALSE))
  if (is_skim_df(x) && nrow(x) > 0) {
    if (include_summary) {
      print(summary(x), .summary_rule_width = summary_rule_width, ...)
    }
    by_type <- partition(x)
    purrr::map(
      by_type, print, n, width, n_extra, strip_metadata,
      .rule_width = rule_width, ...
    )
    invisible(NULL)
  } else {
    NextMethod("print")
  }
}

#' @describeIn print Print an entry within a partitioned `skim_df`.
#' @param .rule_width Width for the rule above the skim results for each type.
#' @param .width Width for the tibble for each type.
#' @export
print.one_skim_df <- function(x,
                              n = Inf,
                              .width = Inf,
                              n_extra = NULL,
                              strip_metadata = getOption(
                                "skimr_strip_metadata", FALSE
                              ),
                              .rule_width = base::options()$width,
                              ...) {
  variable_type <- paste("Variable type:", attr(x, "skim_type"))
  top_line <- cli::rule(line = 1, left = variable_type, width = .rule_width)
  out <- format(x, ..., n = n, width = .width, n_extra = n_extra)
  if (strip_metadata) {
    metadata <- -1 * grab_tibble_metadata(out)
  } else {
    metadata <- seq_along(out)
  }
  render_skim_body(top_line, out, metadata)
}

grab_tibble_metadata <- function(x) {
  if (crayon::has_color()) {
    grep("^\\s*\\\033\\[38;5;\\d{3}m[#\\*]", x)
  } else {
    grep("^\\s*[#\\*]", x)
  }
}

render_skim_body <- function(top_line, out, metadata_to_remove) {
  cat(paste0("\n", top_line), out[metadata_to_remove], sep = "\n")
}

#' @describeIn print Print a `skim_list`, a list of `skim_df` objects.
#' @export
print.skim_list <- function(x, n = Inf, width = Inf, n_extra = NULL,
                            .rule_width = base::options()$width, ...) {
  nms <- names(x)
  attributes(x) <- NULL
  print(rlang::set_names(x, nms), rule_width = .rule_width)
}


#' @describeIn print Print method for a `summary_skim_df` object.
#' @param .summary_rule_width the width for the main rule above the summary.
#' @export
print.summary_skim_df <- function(x, .summary_rule_width = 40, ...) {
  cat(paste0(cli::rule(
    line = 1, left = "Data Summary",
    width = .summary_rule_width
  ), "\n"))
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
#' @param options Options passed into the print function.
#' @return A `knit_asis` object. Which is used by `knitr` when rendered.
#' @importFrom knitr knit_print
#' @name knit_print
NULL

#' @describeIn knit_print Default `knitr` print for `skim_df` objects.
#' @export
knit_print.skim_df <- function(x, options = NULL, ...) {
  if (is_skim_df(x) && nrow(x) > 0) {
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
    by_type,
    digits = options$skimr_digits %||% 2
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
