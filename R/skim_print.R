#' Manages print for skim_df objects.
#' 
#' A number of common types of data are handled, with classes without 
#' handling falling back to character.
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
  dplyr::do(grouped, skim_print(., grps))
  x
}

#' Print the set of statistics for each typ
#' @keywords internal
#' @noRd

skim_print <- function(.data, groups) {
  skim_type <- .data$type[1]
  funs_used <- get_funs(skim_type)
  fun_names <- names(funs_used)
  collapsed <- collapse_levels(.data, groups)
  wide <- tidyr::spread(collapsed, "stat", "formatted")
  if (options$formats$.align_decimal) {
    wide[fun_names] <- lapply(wide[fun_names], align_decimal)
  }
  
  cat("\nVariable type:", skim_type, "\n")
  var_order <- c(as.character(groups), "variable", fun_names)
  print(structure(wide[var_order], class = "data.frame"))
}

collapse_levels <- function(.data, groups) {
  all_groups <- c(groups, rlang::sym("variable"), rlang::sym("stat"))
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

#' @export
print.spark <- function(x, ...) {
  cat(x, "\n", sep = "")
}
