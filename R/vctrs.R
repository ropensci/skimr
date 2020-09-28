#' Functions for working with the vctrs package
#' Functions for working with the vctrs package
#'
#' These make it clear that we need to use the tibble behavior when joining,
#' concatenating or casting `skim_df` objects. For a discussion, see:
#' <https://github.com/r-lib/vctrs/issues/982>.
#'
#' `vec_ptype2.*` handles finding common prototypes between `skim_df` and
#' similar objects. `vec_cast.*` handles casting between objects.
#'
#' @import vctrs
#' @name skimr-vctrs
#' @keywords internal
NULL

#' @rdname skimr-vctrs
#' @method vec_ptype2 skim_df
#' @export
#' @export vec_ptype2.skim_df
vec_ptype2.skim_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.skim_df")
}

# TODO(michaelquinn32): Change this back to tibbles when we can import
# vec_ptype2.tbl_df and vec_cast.tbl_df.
#' @rdname skimr-vctrs
#' @method vec_ptype2.skim_df skim_df
#' @export
vec_ptype2.skim_df.skim_df <- function(x, y, ...) {
  vec_ptype2(as.data.frame(x), as.data.frame(y))
}

#' @rdname skimr-vctrs
#' @method vec_ptype2.skim_df data.frame
#' @export
vec_ptype2.skim_df.data.frame <- function(x, y, ...) {
  vec_ptype2(as.data.frame(x), y)
}

#' @rdname skimr-vctrs
#' @method vec_ptype2.skim_df tbl_df
#' @export
vec_ptype2.skim_df.tbl_df <- function(x, y, ...) {
  vec_ptype2(tibble::as_tibble(x), y)
}

#' @rdname skimr-vctrs
#' @method vec_ptype2.data.frame skim_df
#' @export
vec_ptype2.data.frame.skim_df <- function(x, y, ...) {
  vec_ptype2(x, as.data.frame(y))
}

#' @rdname skimr-vctrs
#' @method vec_cast skim_df
#' @export
#' @export vec_cast.skim_df
vec_cast.skim_df <- function(x, to, ...) {
  UseMethod("vec_cast.skim_df")
}

#' @rdname skimr-vctrs
#' @method vec_cast.skim_df tbl_df
#' @export
vec_cast.skim_df.skim_df <- function(x, to, ...) {
  x
}

#' @rdname skimr-vctrs
#' @method vec_cast.skim_df tbl_df
#' @export
vec_cast.skim_df.tbl_df <- function(x, to, ...) {
  tibble::as_tibble(x)
}

#' @rdname skimr-vctrs
#' @method vec_cast.skim_df data.frame
#' @export
vec_cast.skim_df.data.frame <- function(x, to, ...) {
  as.data.frame(x)
}

#' @rdname skimr-vctrs
#' @method vec_cast.data.frame skim_df
#' @export
vec_cast.data.frame.skim_df <- function(x, to, ...) {
  as.data.frame(x)
}
