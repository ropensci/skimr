#' Functions for accessing skim_df attributes
#'
#' These functions simplify access to attributes contained within a `skim_df`.
#' While all attributes are read-only, being able to extract this information
#' is useful for different analyses. These functions should always be preferred
#' over calling base R's attribute functions.
#'
#' @param object A `skim_df` or `skim_list`.
#' @return Data contained within the requested `skimr` attribute.
#' @name skim-attr
NULL

#' @describeIn skim-attr Get the number of rows in the skimmed data frame.
#' @export
data_rows <- function(object) {
  attr(object, "data_rows")
}

#' @describeIn skim-attr Get the number of columns in the skimmed data frame.
#' @export
data_cols <- function(object) {
  attr(object, "data_cols")
}

#' @describeIn skim-attr Get the name of the skimmed data frame. This is only
#'   available in contexts where the name can be looked up. This is often not
#'   the case within a pipeline.
#' @export
df_name <- function(object) {
  attr(object, "df_name")
}

#' @describeIn skim-attr Get the names of the groups in the original data frame.
#'   Only available if the data was grouped. Otherwise, `NULL`.
#' @export
group_names <- function(object) {
  attr(object, "groups")
}

#' @describeIn skim-attr Get the names of the base skimming functions used.
#' @export
base_skimmers <- function(object) {
  attr(object, "base_skimmers")
}

#' @describeIn skim-attr Get the names of the skimming functions used, separated
#'   by data type.
#' @export
skimmers_used <- function(object) {
  attr(object, "skimmers_used")
}


#' Test if an object is compatible with `skimr`
#'
#' Objects within `skimr` are identified by a class, but they require additional
#' attributes and data columns for all operations to succeed. These checks help
#' ensure this. While they have some application externally, they are mostly
#' used internally.
#'
#' Most notably, a `skim_df`
#'
#' * has columns `skim_type` and `skim_variable`
#' * has more than zero rows
#'
#' And has the following special attributes
#'
#' * `data_rows`: n rows in the original data
#' * `data_cols`: original number of columns
#' * `df_name`: name of the original data frame
#' * `groups`: if there were group variables
#' * `base_skimmers`: names of functions applied to all skim types
#' * `skimmers_used`: names of functions used to skim each type
#'
#' @param object Any `R` object.
#' @name skim-obj
NULL

#' @describeIn skim-obj Does the object have the `skim_type` column?
#' @export
has_type_column <- function(object) {
  "skim_type" %in% names(object)
}

#' @describeIn skim-obj Does the object have the `skim_variable` column?
#' @export
has_variable_column <- function(object) {
  "skim_variable" %in% names(object)
}

#' @describeIn skim-obj Does the object have the appropriate `skimr` attributes?
#' @export
has_skimr_attributes <- function(object) {
  skimr_attrs <- c(
    "data_rows", "data_cols", "df_name", "groups", "base_skimmers",
    "skimmers_used"
  )
  all(skimr_attrs %in% skimr_attrs)
}

#' @describeIn skim-obj Is the object a `skim_df`?
#' @export
is_skim_df <- function(object) {
  has_type_column(object) && has_variable_column(object) &&
    has_skimr_attributes(object) && nrow(object) > 0
}

#' @describeIn skim-obj Stop if the object is not a `skim_df`.
#' @export
assert_is_skim_df <- function(object) {
  stopifnot(
    has_type_column(object), has_variable_column(object),
    has_skimr_attributes(object), nrow(object) > 0
  )
  object
}

#' @describeIn skim-obj Is the object a `skim_list`?
#' @export
is_skim_list <- function(object) {
  have_variable_column <- purrr::map_lgl(object, has_variable_column)
  all(have_variable_column) && has_skimr_attributes(object)
}

#' @describeIn skim-obj Stop if the object is not a `skim_list`.
#' @export
assert_is_skim_list <- function(object) {
  have_variable_column <- purrr::map_lgl(object, has_variable_column)
  stopifnot(all(have_variable_column), has_skimr_attributes(object))
  object
}

#' @describeIn skim-obj Is this a data frame with `skim_variable` and
#'  `skim_type` columns?
#' @export
could_be_skim_df <- function(object) {
  is.data.frame(object) && has_variable_column(object) &&
    has_type_column(object) && nrow(object) > 0
}

#' Remove `skimr` class if not needed.
#' @noRd
strip_skim_attrs <- function(object) {
  attrs <- attributes(object)
  stripped <- purrr::list_modify(
    attrs,
    class = class(object)[-1],
    data_rows = NULL,
    data_cols = NULL,
    df_name = NULL,
    base_skimmers = NULL,
    skimmers_used = NULL
  )
  attributes(object) <- stripped
  object
}

#' Pass attributes from a `skimr` object to a new object.
#' @noRd
reassign_skim_attrs <- function(object, skim_df, ...) {
  defaults <- list(
    class = c("skim_df", "tbl_df", "tbl", "data.frame"),
    data_rows = data_rows(skim_df),
    data_cols = data_cols(skim_df),
    df_name = df_name(skim_df),
    groups = group_names(skim_df),
    base_skimmers = base_skimmers(skim_df),
    skimmers_used = skimmers_used(skim_df)
  )
  updated <- purrr::list_modify(defaults, ...)
  assign_new_attributes(object, !!!updated)
}

assign_new_attributes <- function(object, ...) {
  original <- attributes(object)
  attributes(object) <- purrr::list_modify(original, ...)
  object
}
