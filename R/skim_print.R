#' Print `skim` objects
#' 
#' @param x Either a `skim_df`, `skim_vector` or `skim_summary` object.
#' @param ... Further arguments passed to or from other methods.
#' @name print
NULL


#' @describeIn print Prints a skimmed data frame (`skim_df` from [`skim()`])
#' @export

print.skim_df <- function(x, ...) {
  defaults <- options(dplyr.show_progress = FALSE)
  on.exit(options(defaults))
  
  cat("Skim summary statistics\n")
  cat(" n obs:", attr(x, "data_rows"), "\n")
  cat(" n variables:", attr(x, "data_cols"), "\n")

  grps <- dplyr::groups(x) 
  if (!is.null(grps)) {
    flat <- paste(grps, collapse = ", ")
    cat(" group variables:", flat, "\n")
  }
  
  grouped <- dplyr::group_by(x, !!rlang::sym("type"))
  dplyr::do(grouped, skim_render(., grps, print_impl, ...))
  invisible(x)
}


#' @describeIn print Manages print for `skim_vector` objects.
#' @export

print.skim_vector <- function(x, ...) {
  cat("Skim summary statistics\n")
  skim_render(x, groups = as.null(), print_impl, ...)
  
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
      type_frequency_string
      ,sep = "")
}

#' Print expanded skim tables with a simple caption
#' @keywords internal
#' @noRd

print_impl <- function(transformed_df, skim_type, ...) {
  cat("\nVariable type:", skim_type, "\n")
  mat <- as.matrix(transformed_df)
  dimnames(mat)[[1]] <- rep("", nrow(mat))
  print(enc2utf8(mat), quote = FALSE, right = TRUE)
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
  
  var_order <- c(as.character(groups), "variable", fun_names)
  FUN(wide[var_order], skim_type, ...)
}

collapse_levels <- function(.data, groups) {
  all_groups <- c(groups, rlang::syms(c("variable", "stat")))
  grouped <- dplyr::group_by(.data, !!!all_groups)
  dplyr::summarize(grouped, formatted = collapse_one(.data$formatted))
}

collapse_one <- function(vec) {
  len <- min(length(vec), options$formats$.levels$max_levels)
  paste(vec[seq_len(len)], collapse = ", ")
}

align_decimal <- function(x) {
  split <- stringr::str_split(x, "\\.", simplify = TRUE)
  if (ncol(split) < 2) return(x)
  max_whole <- max(nchar(split[,1]))
  max_decimal <- max(nchar(split[,2]))
  left <- stringr::str_pad(split[,1], max_whole, side = "left")
  right <- stringr::str_pad(split[,2], max_decimal, side = "right")
  dec <- ifelse (split[, 2] == "", " ", ".") 
  sprintf("%s%s%s", left, dec, right)
}

#' @export
print.spark <- function(x, ...) {
  cat(x, "\n", sep = "")
}
