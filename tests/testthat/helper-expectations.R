# Simpler expectations for data frames.

expect_n_columns <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  act <- quasi_label(enquo(object))
  act$n <- length(act$val)
  expect(act$n == n, sprintf("%s has %i columns, not %i columns.", 
                             act$lab, act$n, n))
  invisible(act$val)
}

expect_n_rows <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  act <- quasi_label(enquo(object))
  act$n <- nrow(act$val)
  expect(act$n == n, sprintf("%s has %i rows, not %i rows",  act$lab, act$n, n))
  invisible(act$val)
}
