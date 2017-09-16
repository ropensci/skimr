#' Extract summary statistics for vector.
#' 
#' The set of functions used to summarize the vector depends on the vectors
#' type. See \code{\link{skim_with}} and
#' \code{\link{skim_format}} for how \code{skim_v} can be customized.
#'
#' @param x A vector.
#' @param vector_type A character vector that specifies which group of funs
#'  to grab for summarizing.
#' @return A tall tbl, containing the vector's name, type, potential levels
#'  and a series of summary statistics.
#' @keywords internal
#' @export

skim_v <- function(x, vector_type = class(x)) {
  stopifnot(length(x) > 0,
            is.character(vector_type))
  funs <- get_funs(vector_type)

  if (is.null(funs)) {
    collapsed <- paste(class(x), collapse = ", ")
    msg <- paste("No summary functions for vectors of class:", collapsed)
    warning(msg, ".\nCoercing to character", call. = FALSE)
    
    funs <- get_funs("character")
    vector_type <- "character"
    x <- as.character(x)
  }

  # Compute the summary statistic; allow for variable length
  values <- purrr::map(funs, ~.x(x))
  
  # Assert that the values calculated above can be used in output, i.e. the
  # functions don't create anything list-like
  check <- purrr::map_lgl(values, ~is.recursive(.x))
  if (any(check)) {
    collapsed <- paste(names(funs)[check], collpase = ", ")
    stop("Functions for class ", get_vector_type_used(vector_type),
         " did not return atomic vectors: ", collapsed)
  }
  
  # Assert that any statistic vector longer than 1 has names
  # use these names to get levels
  nms <- purrr::map(values, ~names(.x))
  check <- purrr::map2_lgl(values, nms, check_levels)
  if (any(check)) {
    collapsed <- paste(names(funs)[check], collapse = ", ")
    stop("Levels missing from the following functions: ", collapsed)
  }
  level <- purrr::map_if(nms, is.null, ~".all")
  
  # Get the formatted version of the computed values and standardize raw values
  # Within skimr, summary functions can return vectors of any type. Character
  # values are used to produced formatted output, while all other type are
  # coerced to numeric.
  formats <- purrr::map(values, get_formatted)
  values_out <- purrr::map_if(values, is.character, ~NA_real_)

  # Produce output, the lens vector is used to ensure that vectors within the
  # tibble all have the same length
  lens <- lengths(values, use.names = FALSE)
  tibble::tibble(type = get_vector_type_used(vector_type), 
    stat = rep(names(funs), lens),
    level = purrr::flatten_chr(level), 
    value = unlist(values_out, use.names = FALSE),
    formatted = purrr::flatten_chr(formats))
}

check_levels <- function(values, names) {
  if (length(values) > 1) {
    complete <- !is.na(names)
    empty <- names[complete] == ""
    null <- purrr::map_lgl(names[complete], is.null)
    any(empty | null)
  } else {
    FALSE
  }
}
