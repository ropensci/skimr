#' Manages print for skim objects.
#' 
#' Currently, numeric, factor and character data are handled.
#' 
#' @param x A \code{skim_df} object
#' @importFrom dplyr select mutate filter data_frame left_join
#' @importFrom tidyr gather spread
#' @return A \code{skim_df} object
#' @export
skim_print <- function(x){
  nums_dt <- x[x$type %in% c("numeric", "double", "integer"),]
  fcts_dt <- x[x$type == "factor",]
  chars_dt <- x[x$type == "character",]
  
  nums <- NULL
  fcts <- NULL
  chars <- NULL
  
  if (nrow(nums_dt) > 0) {
    nums <- formatnum(nums_dt)
  }
  if (nrow(fcts_dt) > 0) {
    fcts <- formatfct(fcts_dt)
  }
  if (nrow(chars_dt) > 0) {
    chars <- formatchars(chars_dt)
  }
  return(list(numeric = nums, factors = fcts, characters=chars))
}

formatnum <- function(x) {
  tmp1 <- dplyr::mutate(x, stat = ifelse(stat == "quantile", 
                               paste(level, stat), stat))
  tmp1 <- dplyr::select(dplyr::filter(tmp1, stat != "hist"), -level)
  tmp1 <- dplyr::mutate(tmp1, stat = factor(stat, levels = c(
    "missing", "complete", "n", "mean", "sd", "min", "25% quantile", "median",
    "75% quantile", "max", "hist"
  )))
  tmp1 <- tidyr::spread(tmp1, stat, value)
  
  tmp2 <- dplyr::mutate(dplyr::filter(x, stat == "hist"), value = level)
  tmp2 <- tidyr::spread(dplyr::select(tmp2, -level), stat, value)
  
  tmp <- dplyr::left_join(tmp1, tmp2, by = c("var", "type"))
  return(tmp)
}

formatfct <- function(x) {
  tmp <- x[x$level == ".all",]
  tmp <- tmp[complete.cases(tmp),]
  tmp <- tidyr::spread(dplyr::select(tmp, -level), stat, value)
  tmp1 <- x[x$stat == "count",]

  wide <- tibble::data_frame()
  for (i in unique(x$var)) {
    tmp2 <- tmp1[tmp1$var == i,]
    tmp2$stat <- paste0(tmp2$level, ": ", tmp2$value)
    wide <- rbind(wide, tibble::data_frame(var = i, 
                                   type = "factor", 
                                   stat = paste0(tmp2$stat, collapse = " ")))
  }
  tmp <- dplyr::left_join(tmp, wide, by = c("var", "type"))
  
  return(tmp)
}

formatchars <- function(x) {
  tmp1 <- dplyr::mutate(x, stat = ifelse(stat == "quantile", 
                                         paste(level, stat), stat))
  tmp1 <- dplyr::select(dplyr::filter(tmp1, stat != "hist"), -level)
  tmp1 <- dplyr::mutate(tmp1, stat = factor(stat, levels = c(
    "complete","missing", "empty", "n", "min","max", "n_unique" 
  )))
  tmp1 <- tidyr::spread(tmp1, stat, value)
  
  return(tmp1)
}

#' @export
print.skim_df <- function(x, ...) {
  wide_values <- skim_print(x)
  
  if (! is.null(wide_values$numeric)) {
    cat("Numeric Variables\n")
    print(wide_values$numeric, ...)
  }
  
  if (! is.null(wide_values$characters)) {
    cat("\nCharacter Variables\n")
    print(wide_values$characters, ...)
  }
  
  if (! is.null(wide_values$factors)) {
    cat("\nFactor Variables\n")
    print(wide_values$factors, ...)
  }
}
