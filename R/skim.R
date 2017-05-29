#' Get useful summary statistic from a data frame
#' 
#' @param .data A tbl, or an object that can be coerced into a tbl.
#' @return A \code{skim_df} object, which can be treated like a
#'   tbl in most instances.
#' @export

skim <- function(.data) {
  UseMethod("skim")
}

#'@export

skim.data.frame <- function(.data) {
  if (dplyr::is.grouped_df(.data)) {
    skim_df <- .data %>% 
      tidyr::nest() %>% 
      dplyr::mutate(stats = purrr::map(data, skim)) %>% 
      dplyr::mutate(stats = purrr::map(stats, 
                                       append_group_vars, 
                                       groups = dplyr::groups(.data)))
    combined <- dplyr::bind_rows(skim_df$stats)
  } else {
    rows <- purrr::map(.data, skim_v)
    combined <- dplyr::bind_rows(rows, .id = "var")
  }
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
#' A data frame with the grouping variables in new columns, labeled by "group_var_1", 
#' "group_var_2" etc. 
#'
#' @examples
append_group_vars <- function(df, groups){
  group_char <- dplyr::data_frame(x = as.character(groups)) %>% 
    t() %>% 
    dplyr::as_data_frame()
  group_var_df <- purrr::map(1:nrow(df), ~group_char) %>% dplyr::bind_rows()
  names(group_var_df) <- paste0("group_var_", seq_along(groups))
  dplyr::bind_cols(df, group_var_df)
}