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
  
  grps <- dplyr::groups(x) 
  if (!is.null(grps)) {
    flat <- paste(grps, collapse = ", ")
    cat(" group variables:", flat, "\n")
  }
  
  grouped <- dplyr::group_by(x, type)
  dplyr::do(grouped, skim_print(., grps))
  x
}

#' Print the set of statistics for each typ
#' @keywords internal
#' @noRd

skim_print <- function(.data, groups) {
  skim_type <- .data$type[1]
  funs_used <- get_funs(skim_type)
  collapsed <- collapse_levels(.data, groups)
  wide <- tidyr::spread(collapsed, "stat", "formatted")
  cat("\nVariable type:", skim_type, "\n")
  print(structure(wide[c(as.character(groups), "var", names(funs_used))],
                  class = "data.frame"))
}

collapse_levels <- function(.data, groups) {
  all_groups <- c(groups, rlang::sym("var"), rlang::sym("stat"))
  grouped <- dplyr::group_by(.data, !!!all_groups)
  dplyr::summarize(grouped, formatted = collapse_one(formatted))
}

collapse_one <- function(vec) {
  len <- min(length(vec), options$formats$.levels$max_levels)
  paste(vec[seq_len(len)], collapse = ", ")
}
