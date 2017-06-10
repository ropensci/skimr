#' Get useful summary statistic from a data frame
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'   tbl in most instances.
#' @importFrom magrittr %>%
#' @export

skim <- function(.data) {
  UseMethod("skim")
}

#' Get useful summary statistic from a data frame, print it, and return it
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'   tbl in most instances.
#' @export
skim_tee <- function(.data) {
  t <- skim(.data)
  print(t)
  invisible(.data)
}

#'@export

skim.data.frame <- function(.data) {
  rows <- purrr::map(.data, skim_v)
  combined <- dplyr::bind_rows(rows, .id = "var")
  return(structure(combined, class = c("skim_df", class(combined))))
}

#' Title
#'
#' @param .data 
#'
#' @return
#' A grouped data frame
#' @export
#'
#' @examples
#' mtcars %>% group_by(cyl, gear) %>% skim()
#' 
skim.grouped_df <- function(.data){
  nested_df <- .data %>%   
    tidyr::nest() 
  groups <-  as.character(dplyr::groups(.data))
  
  l <- split(nested_df[, groups], factor(1:nrow(nested_df))) 
  l <- purrr::map( l, ~dplyr::combine(.))
  skim_df <- nested_df %>% 
    dplyr::mutate(stats = purrr::map(data, skim)) %>% 
    dplyr::mutate(stats = purrr::map2(stats, 
                                      l,
                                      ~append_group_vars(.x, .y, groups = groups)))
  combined <- dplyr::bind_rows(skim_df$stats) %>% 
    dplyr::group_by_(.dots = groups)
  return(structure(combined, class = c("skim_df", class(combined))))
}

#' Append Grouping Variables
#'
#' @param df 
#' A \code{skim_df}
#' @param groups 
#' The grouping variables of the data frame to be summarized
#'
#' @return
#' A data frame with the grouping variables in new columns
#'
#' @examples
append_group_vars <- function(df, val, groups){
  group_val <- dplyr::data_frame(x = val) %>% 
    t() %>% 
    dplyr::as_data_frame()
  group_var_df <- purrr::map(1:nrow(df), ~group_val) %>% dplyr::bind_rows()
  names(group_var_df) <-  groups
  dplyr::bind_cols(group_var_df, df)
}
