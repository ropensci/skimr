# Simpler expectations for data frames.

expect_n_columns <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  act <- testthat::quasi_label(rlang::enquo(object))
  act$n <- length(act$val)
  testthat::expect(act$n == n, sprintf(
    "%s has %i columns, not %i columns.",
    act$lab, act$n, n
  ))
  invisible(act$val)
}

expect_n_rows <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  act <- testthat::quasi_label(rlang::enquo(object))
  act$n <- nrow(act$val)
  testthat::expect(
    act$n == n,
    sprintf("%s has %i rows, not %i rows", act$lab, act$n, n)
  )
  invisible(act$val)
}

expect_NA <- function(object) {
  act <- testthat::quasi_label(rlang::enquo(object))
  testthat::expect(is.na(act$val), sprintf("%s is not NA", act$lab))
  invisible(act$val)
}
