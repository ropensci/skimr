#' Mutate a skim_df
#' 
#' [dplyr::mutate()] older than version 0.8.0 drops attributes, but we need to keep 
#' them around for other skim behaviors and consistency with newer versions of dplyr. 
#' Otherwise the behavior is exactly the same as dplyr 0.8.0. For more
#' information, see https://github.com/tidyverse/dplyr/issues/3429. 
#'
#' @param .data A `skim_df`, which behaves like a `tbl.`
#' @param ... Name-value pairs of expressions, each with length 1 or the same
#'   length as the number of rows in the group (if using [group_by()] or in the
#'   entire input (if not using groups). The name of each argument will be the
#'   name of a new variable, and the value will be its corresponding value. 
#'   Use `NULL` value in `mutate` to drop a variable.  New variables overwrite
#'   existing variables of the same name.
#'   
#'   The arguments in `...` are automatically [quoted](rlang::quo) and
#'   [evaluated](rlang::eval_tidy) in the context of the data frame. They support
#'   [unquoting](rlang::quasiquotation) and splicing. See
#'   `vignette("programming", package = "dplyr")` for an introduction to these
#'   concepts.
#' @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. In many ways, the object behaves like a [tibble::tibble()].
#' @seealso [dplyr::mutate()] for the function's expected behavior.
#' @export
mutate.skim_df <- function(.data, ...) {
  mutated <- NextMethod("mutate")
  rebuild_skim_obj(mutated, .data)
}

#'  Transmute a skim_df
#'  
#' [dplyr::transmute()] older than version 0.8.0 drops attributes, but we need to keep 
#' them around for other skim behaviors and consistency with newer versions of dplyr. 
#' Otherwise the behavior is exactly the same as dplyr 0.8.0. For more
#' information, see https://github.com/tidyverse/dplyr/issues/3429. We also need to
#' retain the two metadata columns (`variable` and `type`) that are the defining 
#' features of a skim_df object.
#'
#' @param .data A `skim_df`, which behaves like a `tbl.`
#' @param ... Name-value pairs of expressions, each with length 1 or the same
#'   length as the number of rows in the group (if using [group_by()] or in the
#'   entire input (if not using groups). The name of each argument will be the
#'   name of a new variable, and the value will be its corresponding value. 
#'   Use `NULL` value in `mutate` to drop a variable.  New variables overwrite
#'   existing variables of the same name.
#'   @return A `skim_df` object, which also inherits the class(es) of the input
#'   data. The `variable`` and `type` variables will be included
#'   even if not named. 
#'   In many ways, the object behaves like a [tibble::tibble()].
#' @seealso [dplyr::transmute()] for the function's expected behavior.
#' @export
transmute.skim_df <- function(.data, ...) {
  transmuted <- NextMethod("transmute")
  transmuted <- transmuted[!colnames(transmuted) %in% c("variable", "type")]
  base_skim <- .data[c("variable", "type")]
  transmuted <- cbind(base_skim, transmuted)
  rebuild_skim_obj(transmuted, .data)
}

#' Use dplyr verb filter on skim_df objects.
#' @seealso [`dplyr::filter()`]
#' @inheritParams dplyr::filter
#' @return  skim_df object coerced to a data frame.
#' @export
filter.skim_df <-function (.data, ..., .preserve = FALSE) {
  filtered <-  NextMethod("filter")
  rebuild_skim_obj(filtered, .data)
}
#
#' Use dplyr verb select on skim_df objects.
#' Always includes variable and type columns even if user has not selected them.
#' @seealso [`dplyr::select()`]
#' @inheritParams dplyr::select
#' @return  skim_df object
#' @export
select.skim_df <-function (.data, ...) {
   mc <- match.call()
   mc[[length(mc) +1]] <- quote(variable)
   mc[[length(mc) +1]] <- quote(type)
   selected <-  NextMethod("select")
   rebuild_skim_obj(selected, .data)
}

#' Use dplyr verb arrange on skim_df objects.
#' #' @seealso [`dplyr::arrange()`]
#' @inheritParams dplyr::arrange
#' @return  skim_df object coerced to a data frame.
#' @export
arrange.skim_df <-function (.data, ...) {
   arranged <-  NextMethod("arrange")
   rebuild_skim_obj(arranged, .data)
}

#' Use dplyr verb slice on skim_df objects.
#' @seealso [`dplyr::slice()`]
#' @inheritParams dplyr::filter
#' @return  skim_df object coerced to a data frame.
#' @export
slice.skim_df <-function (.data, ..., .preserve = FALSE) {
  sliced <- NextMethod("slice")
  rebuild_skim_obj(sliced, .data)
}


