#' Working with `skimr`'s printed output
#' 
#' These functions provide two approaches for handling the wide format produced
#' when you print `skim_df`. `skim_to_wide()` returns a wide data frame with one
#' row per variable and `NA` for statistics not calculated for a given type.
#' `skim_to_list()` creates a list of wide tibbles, one for each type of vector
#' within your data frame.
#' 
#' Note that in both cases, all columns are character vectors. This gives you
#' additional control of the printed output, but not the original data.
#' 
#' @param x A \code{dataframe}.
#' @param ... Further arguments passed to or from other methods.
#' @return A wide data frame or a list of wide data frames.
#' @examples 
#' # Treat the printed output as a wide data frame
#' skim_to_wide(iris)
#' iris %>% skim_to_wide()
#' iris %>%
#'   skim_to_wide() %>%
#'   dplyr::filter(type == "factor") %>% 
#'   dplyr::select(top_counts)
#'
#' # Treat the printed output as a list of data frames
#' skim_to_list(iris)
#' iris %>% skim_to_list()
#' 
#' # Save the result
#' sl <- iris %>% skim_to_list() 
#' sl[["numeric"]]
#' kable(sl$numeric)
#' 
#' # Or grouped, this uses the magrittr exposition pipe
#' # see ?magrittr::`%$%`
#' library(magrittr)
#' iris %>%
#'   dplyr::group_by(Species) %>%
#'   skim_to_list() %$%
#'   kable(numeric)
#' @name wide
NULL


#' @rdname wide
#' @export

skim_to_wide <- function(x, ...) {
  x <- skim(x)
  grps <- dplyr::groups(x)
  grouped <- dplyr::group_by(x, !!rlang::sym("type"))
  x <- dplyr::do(grouped, skim_render(., grps, quiet_impl, ...))
  dplyr::ungroup(x)
}


#' @rdname wide
#' @export

skim_to_list <- function(x, ...){
  x <- skim(x, ...)
  grps <- dplyr::groups(x)
  separate <- split(x, x$type)
  purrr::map(separate, skim_render, grps, quiet_impl)
}


#' Expand skim tables without printing
#' @keywords internal
#' @noRd

quiet_impl <- function(transformed_df, skim_type, ...) {
  structure(transformed_df, class = c( "tbl", "tbl_df", "data.frame"))
}
