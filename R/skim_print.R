#' Print a skimmed data frame.
#' 
#' @param x A \code{skim_df} object.
#' @param ... Further arguments passed to or from other methods.
#' @return The original \code{skim_df} object.
#' @export

print.skim_df <- function(x, ...) {
  cat("Skim summary statistics\n")
  cat(" n obs:", attr(x, "data_rows"), "\n")
  cat(" n variables:", attr(x, "data_cols"), "\n")

  grps <- dplyr::groups(x) 
  if (!is.null(grps)) {
    flat <- paste(grps, collapse = ", ")
    cat(" group variables:", flat, "\n")
  }
  
  grouped <- dplyr::group_by_(x, ~type)
  dplyr::do(grouped, skim_render(., grps, print_impl, ...))
  invisible(x)
}

#' Print expanded skim tables with a simple caption
#' @keywords internal
#' @noRd

print_impl <- function(transformed_df, skim_type, ...) {
  cat("\nVariable type:", skim_type, "\n")
  print(structure(transformed_df, class = "data.frame"), ...)
}


#' @export

kable <- function (x, format = NULL, digits = getOption("digits"), row.names = NA, 
                   col.names = NA, align, caption = NULL, format.args = list(), 
                   escape = TRUE, ...) {
  UseMethod("kable")
}

#' @export

kable.data.frame <- knitr::kable

#' Produce \code{kable} output of a skimmed data frame
#'
#' @seealso \code{\link[knitr]{kable}}
#' @param x an R object (typically a matrix or data frame)
#' @param format a character string; possible values are latex, html,
#'  markdown, pandoc, and rst; this will be automatically determined if the
#'  function is called within knitr; it can also be set in the global option
#'  knitr.table.format; if format is a function, it must return a character
#'  string
#' @param digits the maximum number of digits for numeric columns (passed to
#'  round()); it can also be a vector of length ncol(x) to set the number of
#'  digits for individual columns.
#' @param row.names a logical value indicating whether to include row names;
#'  by default, row names are included if rownames(x) is neither NULL nor
#'  identical to 1:nrow(x)
#' @param col.names a character vector of column names to be used in the table
#' @param align the alignment of columns: a character vector consisting of
#'  'l' (left), 'c' (center) and/or 'r' (right); by default, numeric columns
#'  are right-aligned, and other columns are left-aligned; if align = NULL,
#'  the default alignment is used; alternatively, if length(align) == 1L, the
#'  string will be expanded to a vector of individual letters unless the output
#'  format is LaTeX; for example, 'clc' will be converted to c('c', 'l', 'c')
#' @param caption the table caption that precedes the variable type
#' @param format.args a list of arguments to be passed to format() to format
#'  table values, e.g. list(big.mark = ',')
#' @param escape escape special characters when producing HTML or LaTeX tables
#' @param ... other arguments.
#' @return The original \code{skim_df} object.
#' @export

kable.skim_df <- function(x, format, digits = getOption("digits"), row.names = NA, 
                          col.names = NA, align = NULL, caption = NULL,
                          format.args = list(), escape = TRUE, ...) {
  grps <- dplyr::groups(x) 
  grouped <- dplyr::group_by_(x, ~type)
  dplyr::do(grouped, skim_render(., grps, kable_impl, format, digits, row.names, 
                                 col.names, align, caption, format.args, 
                                 escape, ...))
  invisible(x)
}

kable_impl <- function(transformed_df, skim_type, format, digits, row.names, 
                       col.names, align, caption, format.args, 
                       escape, ...) {
  cat(sprintf("\nVariable type: %s", skim_type))
  if(is.null(align)) align <- rep("l", length(transformed_df))
  print(kable(transformed_df, caption = NULL, align = align, format, digits,
        row.names,  col.names, format.args,  escape, ...))
  transformed_df
}

#' Expand a skim_df and call a printing function on it
#' @keywords internal
#' @noRd

skim_render <- function(.data, groups, FUN, ...) {
  skim_type <- .data$type[1]
  funs_used <- get_funs(skim_type)
  fun_names <- names(funs_used)
  collapsed <- collapse_levels(.data, groups)
  wide <- tidyr::spread(collapsed, "stat", "formatted")
  if (options$formats$.align_decimal) {
    wide[fun_names] <- lapply(wide[fun_names], align_decimal)
  }
  
  var_order <- c(as.character(groups), "var", fun_names)
  FUN(wide[var_order], skim_type, ...)
}

collapse_levels <- function(.data, groups) {
  all_groups <- c(groups, rlang::sym("var"), rlang::sym("stat"))
  grouped <- dplyr::group_by(.data, !!!all_groups)
  dplyr::summarize(grouped, formatted = collapse_one(.data$formatted))
}

collapse_one <- function(vec) {
  len <- min(length(vec), options$formats$.levels$max_levels)
  paste(vec[seq_len(len)], collapse = ", ")
}

align_decimal <- function(x){
  split <- stringr::str_split(x, "\\.", simplify = TRUE)
  if (ncol(split) < 2) return(x)
  max_whole <- max(nchar(split[,1]))
  max_decimal <- max(nchar(split[,2]))
  left <- stringr::str_pad(split[,1], max_whole, side = "left")
  right <- stringr::str_pad(split[,2], max_decimal, side = "right")
  dec <- ifelse (split[, 2] == "", " ", ".") 
  sprintf("%s%s%s", left, dec, right)
}
