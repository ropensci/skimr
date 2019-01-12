# #' Use dplyr verb filter on skim_df objects.
# #' #' @seealso [`dplyr::filter()`]
# #' @inheritParams dplyr::filter
# #' @return  skim_df object coerced to a data frame.
# #' @export
# filter.skim_df <-function (.data, ..., .preserve = FALSE) {
#   .data <- as.data.frame(.data)
#   .data <- dplyr::filter(.data, ...)
#   class(.data)  <- c("tbl_df", "tbl", "data.frame")
#   .data
# }
#
# #' Use dplyr verb select on skim_df objects.
# #' #' @seealso [`dplyr::select()`]
# #' @inheritParams dplyr::select
# #' @return  skim_df object coerced to a data frame.
# #' @export
# select.skim_df <-function (.data, ...) {
#   .data <- as.data.frame(.data)
#   .data <- dplyr::select(.data, ...)
#   class(.data)  <- c("tbl_df", "tbl", "data.frame")
#   .data
# }
#
# #' Use dplyr verb mutate on skim_df objects.
# #' #' @seealso [`dplyr::mutate()`]
# #' @inheritParams dplyr::mutate
# #' @return  skim_df object coerced to a data frame.
# #' @export
# mutate.skim_df <-function (.data, ...) {
#   .data <- as.data.frame(.data)
#   .data <- dplyr::mutate(.data, ...)
#   class(.data)  <- c("tbl_df", "tbl", "data.frame")
#   .data
# }
#
# #' Use dplyr verb arrange on skim_df objects.
# #' #' @seealso [`dplyr::arrange()`]
# #' @inheritParams dplyr::arrange
# #' @return  skim_df object coerced to a data frame.
# #' @export
# arrange.skim_df <-function (.data, ...) {
#   .data <- as.data.frame(.data)
#   .data <- dplyr::arrange(.data, ...)
#   class(.data)  <- c("tbl_df", "tbl", "data.frame")
#   .data
# }
#
# #' Use dplyr verb slice on skim_df objects.
# #' #' @seealso [`dplyr::slice()`]
# #' @inheritParams dplyr::filter
# #' @return  skim_df object coerced to a data frame.
# #' @export
# slice.skim_df <-function (.data, ..., .preserve = FALSE) {
#   .data <- as.data.frame(.data)
#   .data <- dplyr::slice(.data, ...)
#   class(.data)  <- c("tbl_df", "tbl", "data.frame")
#   .data
# }
