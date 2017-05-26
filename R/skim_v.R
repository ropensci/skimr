#' Extract summary statistics for vector
#' 
#' @param x A vector
#' @param FUNS A list of functions to apply to x to compute summary statistics.
#'   each function should return a numeric value.
#' @return A tall tbl, containing the vector's name, type, potential levels
#'   and a series of summary statistics.
#' @keywords internal
#' @export

skim_v <- function (x, FUNS) {
  UseMethod("skim_v")
}


#' @export

skim_v.numeric <- function(x, FUNS = numeric_funs) {
  stats <- names(FUNS)
  values <- purrr::map_dbl(FUNS, ~.x(x))
  out <- tibble::tibble(type = class(x), 
    stat = stats,
    level = NA, 
    value = values)
  return(out)
}



numeric_funs <- list(
  missing = purrr::compose(sum, is.na),
  complete = purrr::compose(sum, `!`, is.na),
  n = length,
  mean = mean,
  q1 = purrr::partial(quantile, probs = .25),
  q3 = purrr::partial(quantile, probs = .75),
  sd = sd,
  median = median,
  max = max,
  min = min
)


skim_v.factor <- function(x) {
  t<-table(x, useNA = "always")
  t<-tibble::as.tibble(t)
  colnames(t)<-c("level", "value")
  t$type <- "factor"
  t$stat <- "count"
  out <- t[c("type", "stat", "level", "value")]
  return(out)
}

skim_v.character <- function(x, FUNS = character_funs) {
  stats <- names(FUNS)
  value <- purrr::map_dbl(FUNS, ~.x(x))
  out <- tibble::tibble(type = class(x), 
                        stat = stats,
                        level = NA, 
                        value = value)
  return(out)
}

character_funs <- list (
  n = length,
  max = purrr::compose(max, nchar),
  min = purrr::compose(min, nchar),
  #blank    = ,
  missing  = purrr::compose(sum, is.na),
  n_unique = purrr::compose(length, unique)
)

