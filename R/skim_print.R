#' Manages print for skim objects.
#' 
#' Currently, numeric, factor and character data are handled.
#' 
#' @param x A \code{skim_df} object.
#' @param ... Further arguments passed to or from other methods.
#' @return The original \code{skim_df} object.
#' @export

print.skim_df <- function(x, ...) {
  cat("Skim summary statistics\n")
  cat(" n obs:", nrow(x), "\n")
  cat(" n variables:", ncol(x), "\n")
  grouped <- dplyr::group_by(x, type)
  dplyr::do(grouped, skim_print(.))
  x
}

#' Print the set of statistics for each typ
#' @keywords internal
#' @noRd

skim_print <- function(.data) {
  skim_type <- .data$type[1]
  funs_used <- get_funs(skim_type)
  collapsed <- collapse_levels(.data)
  wide <- tidyr::spread(collapsed, stat, formatted)
  cat("\nVariable type:", skim_type, "\n")
  print(structure(wide[c("var", names(funs_used))], class = "data.frame"))
}

collapse_levels <- function(df) {
  grouped <- dplyr::group_by(df, var, stat)
  dplyr::summarize(grouped, formatted = collapse_one(formatted))
}

collapse_one <- function(vec) {
  len <- min(length(vec), options$formats$.levels$max_levels)
  paste(vec[seq_len(len)], collapse = ", ")
}