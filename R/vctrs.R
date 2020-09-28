#' Functions for working with the vctrs package
#'
#' These make it clear that we need to use the tibble behavior when joining,
#' concatenating or casting `skim_df` objects. For a discussion, see:
#' <https://github.com/r-lib/vctrs/issues/982>.
#'
#' `vec_ptype2.*` handles finding common prototypes between `skim_df` and
#' similar objects. `vec_cast.*` handles casting between objects. Note that
#' as of `dplyr 1.0.2`, [dplyr::bind_rows()] does not full support combining
#' attributes and [vctrs::vec_rbind()] is preferred instead.
#'
#' @importFrom vctrs vec_ptype2 vec_cast
#' @name skimr-vctrs
#' @keywords internal
NULL

#' @rdname skimr-vctrs
#' @export
vec_ptype2.skim_df.skim_df <- function(x, y, ...) {
  combine_compatible_objects(x, y, ...)
}

#' @rdname skimr-vctrs
#' @export
vec_ptype2.skim_df.tbl_df <- function(x, y, ...) {
  combine_compatible_objects(x, y, ...)
}

#' @rdname skimr-vctrs
#' @export
vec_ptype2.tbl_df.skim_df <- function(x, y, ...) {
  combine_compatible_objects(x, y, ...)
}

#' @rdname skimr-vctrs
#' @export
vec_cast.skim_df.skim_df <- function(x, to, ...) {
  cast_compatible_object(x, to, ...)
}

#' @rdname skimr-vctrs
#' @export
vec_cast.skim_df.tbl_df <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#' @rdname skimr-vctrs
#' @export
vec_cast.tbl_df.skim_df <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#' We only combine skim_df's that were built with the same set of skimmers.
#' @noRd
has_compatible_skimmers <- function(x, y) {
  attrs_x <- attributes(x) 
  attrs_y <- attributes(y)

  base_x <- attrs_x$base_skimmers %||% attrs_y$base_skimmers
  base_y <- attrs_y$base_skimmers %||% attrs_x$base_skimmers
  used_x <- attrs_x$skimmers_used %||% attrs_y$skimmers_used
  used_y <- attrs_y$skimmers_used %||% attrs_x$skimmers_used

  identical(base_x, base_y) && all(identical_skimmers(used_x, used_y))
}

identical_skimmers <- function(skim_list_x, skim_list_y) {
  x_names <- names(skim_list_x)
  y_names <- names(skim_list_y)
  all_names <- union(x_names, y_names)
  purrr::map_lgl(
    all_names,
    check_identical_skimmers,
    x_names,
    y_names,
    skim_list_x,
    skim_list_y
  )
}

check_identical_skimmers <- function(name,
                                     x_names,
                                     y_names,
                                     skim_list_x,
                                     skim_list_y) {
  if ((name %in% x_names) && (name %in% y_names)) {
    identical(skim_list_x[name], skim_list_y[name])
  } else {
    TRUE
  }
}

combine_compatible_objects <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!has_compatible_skimmers(x, y)) {
    vctrs::stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      details = "Can't combine different sets of skim functions."
    )
  }
  
  if (could_be_skim_df(x) && could_be_skim_df(y)) {
    reassign_skim_attrs(
      vctrs::tib_ptype2(x, y, ...),
      x,
      data_rows = data_rows(x) + data_rows(y),
      data_cols = data_cols(x) + data_cols(y),
      df_name = paste0(df_name(x), "+", df_name(y)),
      groups = c(group_names(x), group_names(y)),
      skimmers_used = union(skimmers_used(x), skimmers_used(y))
    )
  } else {
    strip_skim_attrs(vctrs::tib_ptype2(x, y, ...))
  }
}

cast_compatible_object <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  if (could_be_skim_df(out)) {
    reassign_skim_attrs(out, to)
  } else {
    strip_skim_attrs(out)
  }
}
