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

expect_print_matches_file <- function(object,
                                      filename,
                                      skip_on_windows = getOption(
                                        "skimr_skip_on_windows",
                                        TRUE
                                      ),
                                      skip_on_cran = getOption(
                                        "skimr_skip_on_cran",
                                        TRUE
                                      ),
                                      width = 100,
                                      update = getOption(
                                        "skimr_update_print",
                                        FALSE
                                      ),
                                      ...) {
  if (skip_on_windows) testthat::skip_on_os("windows")
  if (skip_on_windows) testthat::skip_if_not(l10n_info()$`UTF-8`)
  if (skip_on_cran) testthat::skip_on_cran()
  withr::with_options(list(crayon.enabled = FALSE, width = width), {
    testthat::expect_known_output(
      print(object, ...),
      filename,
      update = update,
      width = width
    )
  })
}

expect_matches_file <- function(object,
                                file,
                                update = getOption(
                                  "skimr_update_print",
                                  FALSE
                                ),
                                skip_on_windows = getOption(
                                  "skimr_skip_on_windows",
                                  TRUE
                                ),
                                skip_on_cran = getOption(
                                  "skimr_skip_on_cran",
                                  TRUE
                                ),
                                ...) {
  if (skip_on_windows) testthat::skip_on_os("windows")
  if (skip_on_windows) testthat::skip_if_not(l10n_info()$`UTF-8`)
  if (skip_on_cran) testthat::skip_on_cran()
  act <- testthat::quasi_label(rlang::enquo(object), NULL)

  if (!file.exists(file)) {
    warning("Creating reference value", call. = FALSE)
    writeLines(object, file)
    testthat::succeed()
  } else {
    ref_val <- paste0(readLines(file), collapse = "\n")
    comp <- testthat::compare(as.character(act$val), ref_val, ...)
    if (update && !comp$equal) {
      writeLines(act$val, file)
    }

    testthat::expect(
      comp$equal,
      sprintf(
        "%s has changed from known value recorded in %s.\n%s",
        act$lab, encodeString(file, quote = "'"), comp$message
      ),
      info = NULL
    )
  }

  invisible(act$value)
}
