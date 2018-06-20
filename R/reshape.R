#' Separate a big `skim_df` into smaller data frames, by type.
#' 
#' The data frames produced by [skim()] are wide and sparse, filled with
#' columns that are mostly `NA`. For that reason, it can be convenient to
#' work with "by type" subsets of the original data frame. These smaller
#' subsets have their `NA` columns removed.
#' 
#' @param data A `skim_df`.
#' @param type A character scalar. The subtable to extract from a `skim_df`.
#' @return A `skim_list` of `skim_df`'s, by type.
#' @examples
#' # Create a wide skimmed data frame (a skim_df)
#' skimmed <- skim(iris)
#' 
#' # Separate into a list of subtables by type
#' separate <- partition(skimmed)
#' 
#' # Put back together
#' identical(bind(separate), skimmed)
#' #> TRUE
#' 
#' # Alternatively, get the subtable of a particular type
#' yank(skimmed, "factor")
#' @export
partition <- function(data) {
  as_list <- split(data, data$type)
  skimmers <- attr(data, "skimmers_used")
  groups <- attr(data, "groups")
  reduced <- purrr::imap(as_list, simplify_skimdf, skimmers, groups)
  structure(reduced,
            class = "skim_list",
            data_rows = attr(data, "data_rows"),
            data_cols = attr(data, "data_cols"),
            df_name = attr(data, "df_name"),
            groups = groups,
            skimmers_used = skimmers)
}

simplify_skimdf <- function(data, type, skimmers, groups) {
  keep <- c("variable", groups, skimmers[[type]])
  out <- dplyr::select(data, !!!keep)
  structure(out,
            class = c("one_skim_df", "tbl_df", "tbl", "data.frame"),
            type = type)
}

#' @describeIn partition The inverse of a `partition()`. Rebuild the original
#'   `skim_df`.
#' @export
bind <- function(data) {
  combined <- dplyr::bind_rows(!!!data, .id = "type")
  # The variable column should always be first
  out <- dplyr::select(combined, !!rlang::sym("variable"), dplyr::everything())
  structure(out,
            class = c("skim_df", "tbl_df", "tbl", "data.frame"),
            data_rows = attr(data, "data_rows"),
            data_cols = attr(data, "data_cols"),
            df_name = attr(data, "df_name"),
            skimmers_used = attr(data, "skimmers_used"))
}

#' @describeIn partition Extract a subtable from a `skim_df` with a particular
#'   type.
#' @export
yank <- function(data, type) {
  partition(data)[[type]]
}
