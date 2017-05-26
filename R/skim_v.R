#' @include stats.R
NULL


#' Extract summary statistics for vector
#' 
#' @param x A vector
#' @param FUNS A list of functions to apply to x to compute summary statistics.
#'   Each function should return a numeric value.
#' @return A tall tbl, containing the vector's name, type, potential levels
#'   and a series of summary statistics.
#' @keywords internal
#' @export

skim_v <- function (x, FUNS) {
  UseMethod("skim_v")
}


#' @describeIn skim_v Calculate summary statistics for numeric vectors
#' @export

skim_v.numeric <- function(x, FUNS = numeric_funs) {
  skim_v_(x, FUNS)
}

numeric_funs <- list(
  missing = missing,
  complete = complete,
  n = length,
  mean = purrr::partial(mean, na.rm = TRUE),
  sd = purrr::partial(sd, na.rm = TRUE),
  min = purrr::partial(min, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  quantile = purrr::partial(quantile, probs = c(.25, .75), na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE)
)


#' @describeIn skim_v Calculate summary statistics for factors
#' @export

skim_v.factor <- function(x, FUNS = factor_funs) {
  skim_v_(x, FUNS)
}

factor_funs <- list(
  missing = missing,
  complete = complete,
  n = length,
  count = purrr::partial(table, useNA = "always"),
  n_unique = purrr::compose(length, levels)
)


#' @describeIn skim_v Calculate summary statistics for character vectors
#' @export

skim_v.character <- function(x, FUNS = character_funs) {
  skim_v_(x, FUNS)
}

character_funs <- list (
  missing  = missing,
  complete = complete,
  n = length,
  min = purrr::compose(min, nchar),
  max = purrr::compose(max, nchar),
  #blank    = ,
  n_unique = purrr::compose(length, unique)
)

#' @describeIn skim_v Calculate summary statistics for integer vectors
#' @export

skim_v.integer <- function(x, FUNS = integer_funs) {
  skim_v_(x, FUNS)
}

integer_funs <- numeric_funs


#' @export

skim_v.default <- function(x, FUNS = numeric_funs) {
  msg <- paste0("Skim does not know how to summarize of vector of class: ",
    class(x), ". Coercing to numeric")
  warning(msg, call. = FALSE)
  skim_v(as.numeric(x), FUNS)
}

# Internal implementation of skim_v_. Should work regardless of type.
#
# This enables consistent returns for a variety of functions that generate
# summary statistics. The only difference between the different skim_v methods
# is the functions that they access.
#
# @param x A vector
# @param FUNS A list of functions to apply to x to compute summary statistics.
#   Each function should return a numeric value.
# @return A tall tbl, containing the vector's name, type, potential levels
#   and a series of summary statistics.
# @keywords internal

skim_v_ <- function(x, FUNS) {
  # Compute the summary statistic; allow for variable length
  values <- purrr::map(FUNS, ~.x(x))
  
  # Get the name of the computed statistic and a corresponding level
  lens <- purrr::map_int(values, length)
  stats <- purrr::map2(names(FUNS), lens, rep)
  nms <- purrr::map(values, ~names(.x))
  level <- purrr::map_if(nms, is.null, ~NA)
  
  # Produce output
  tibble::tibble(type = class(x), 
    stat = purrr::flatten_chr(stats),
    level = purrr::flatten_chr(level), 
    value = unlist(values))
}
