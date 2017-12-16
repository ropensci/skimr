globalVariables(".")

#' Get useful summary statistic from a data frame
#' 
#' \code{skim} handles data of all types, dispatching a different set of
#' summary functions based on the types of columns in the data frame.
#' It is an intentionally simple function. See \code{\link{skim_with}} and
#' \code{\link{skim_format}} for how \code{skim} can be customized.
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @param ...  Additional options, normally used to list individual unquoted column names.
#' @return A \code{skim_df} object, which can be treated like a
#'  tbl in most instances.
#' @examples
#' skim(iris)
#' 
#' # Skim also works groupwise
#' dplyr::group_by(iris) %>% skim()
#' @export

skim <- function(.data, ...) {
  UseMethod("skim")
}

#'@export

skim.data.frame <- function(.data, ... ) {
  .vars <- rlang::quos(...)
  if (length(.vars) == 0)  selected <- tidyselect::everything(.data)
  else  selected <- tidyselect::vars_select(names(.data), !!! .vars) 

  rows <- purrr::map(.data[selected], skim_v)
  combined <- dplyr::bind_rows(rows, .id = "variable")
  structure(combined, class = c("skim_df", class(combined)),
            data_rows = nrow(.data), data_cols = length(selected))
}

#' @export

skim.grouped_df <- function(.data, ...) {
  skimmed <- dplyr::do(.data, skim(., ...))
  
  # Drop the grouping variable
  groups <- dplyr::groups(skimmed)
  to_drop <- quote(!(variable %in% groups))
  skimmed <- dplyr::filter(skimmed, !!to_drop)
  structure(skimmed, class = c("skim_df", class(skimmed)),
            data_rows = nrow(.data), data_cols = ncol(.data))
}

#' Print useful summary statistic from a data frame returning the data frame
#'  without modification
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @param ...  Additional options, normally used to list individual unquoted column names.
#' @return The input data frame.
#' @export

skim_tee <- function(.data, ...) {
  print(skim(.data))
  invisible(.data)
}

#' Print skim result and return a single wide data frame of summary statistics
#' 
#'  Returns a wide data frame with one row per variable and NA for statistics
#'  not calculated for a given type. This faciliates future processing.
#' 
#' @param x A \code{dataframe}.
#' @param ... Further arguments passed to or from other methods.
#' @return A wide data frame.
#' @examples 
#'   skim_to_wide(iris)
#'   iris %>% skim_to_wide()
#'   iris %>% skim_to_wide() %>% dplyr::filter(type == "factor") %>% 
#'            dplyr::select(top_counts)
#' @export

skim_to_wide <- function(x, ...) {
  x <- skim(x)
  grps <- dplyr::groups(x)
  grouped <- dplyr::group_by(x, !!rlang::sym("type"))
  x <- dplyr::do(grouped, skim_render(., grps, quiet_impl, ...))
  dplyr::ungroup(x)
}

#' Print skim result and return a list of tibbles
#' 
#'  Returns a list of tibbles (also data frames) with one list element 
#'  per data type. Each column contains the formatted values. 
#'  This facilitates additional processing. 
#'  Note that this is not pipeable.
#' 
#' @param x A \code{dataframe}.
#' @param ... Further arguments passed to or from other methods.
#' @return A list of tibbls.
#' @examples 
#'   skim_to_list(mtcars)
#'   mtcars %>% skim_to_list()
#'   sl <- mtcars %>% skim_to_list() 
#'   sl[["numeric"]]
#' @export

skim_to_list <- function(x, ...){
  x <- skim(x, ...)
  grps <- dplyr::groups(x)
  grouped <- dplyr::group_by(x, !!rlang::sym("type"))
  types <- unique(grouped$type)
  result_list <- list()
   for (t in 1:length(types)){
      result_list[[types[t]]] <- skim_render(dplyr::filter(grouped, !!rlang::sym("type") == types[t]), 
                                                     groups = grps, 
                                                     quiet_impl)
    }
  result_list
  
}
