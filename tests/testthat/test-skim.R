context("Skim a data frame")

test_that("skim returns expected response for numeric vectors", {
  input <- skim(mtcars, mpg)

  # dimensions
  expect_equal(nrow(input), 1)
  expect_equal(length(input), 13)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "mean",
    "sd", "p0", "p25", "p50", "p75", "p100", "hist"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 32)
  expect_equal(attrs$data_cols, 11)
  expect_equal(attrs$df_name, "`mtcars`")
  expect_equal(
    attrs$skimmers_used,
    list(numeric = c(
      "missing", "complete", "n", "mean", "sd", "p0",
      "p25", "p50", "p75", "p100", "hist"
    ))
  )

  # values
  expect_identical(input$skim_variable, "mpg")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$missing, 0L)
  expect_identical(input$complete, 32L)
  expect_identical(input$n, 32L)
  expect_equal(input$mean, 20.1, tolerance = 0.1)
  expect_equal(input$p0, 10.4, tolerance = 0.1)
  expect_equal(input$p25, 15.4, tolerance = 0.1)
  expect_equal(input$p50, 19.2, tolerance = 0.1)
  expect_equal(input$p75, 22.8, tolerance = 0.1)
  expect_equal(input$p100, 33.9, tolerance = 0.1)
  skip_on_os("windows")
  expect_identical(input$hist, "▃▇▅▁▂")
})

test_that("skim handles numeric vectors with NAs and extreme numbers", {
  patho <- tibble::tibble(
    vals = c((2^.Machine$double.digits), NA, -(2^.Machine$double.digits))
  )

  input <- skim(patho)

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`patho`")

  # values
  expect_identical(input$skim_variable, "vals")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$missing, 1L)
  expect_identical(input$complete, 2L)
  expect_identical(input$n, 3L)
  expect_equal(input$mean, 0, tolerance = 1)
  expect_equal(input$sd, 1.27e16, tolerance = 1e14)
  expect_equal(input$p0, -9.01e+15, tolerance = 1e14)
  expect_equal(input$p25, -4.5e+15, tolerance = 1e14)
  expect_equal(input$p50, 0, tolerance = 1)
  expect_equal(input$p75, 4.5e+15, tolerance = 1e14)
  expect_equal(input$p100, 9.01e+15, tolerance = 1e14)
  skip_on_os("windows")
  expect_identical(input$hist, "▇▁▁▁▇")
})

test_that("numeric skim is calculated correctly when x is all NAs.", {
  nas <- as.numeric(c(NA, NA, NA))
  x <- tibble::tibble(nas)
  input <- skim(x)

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")

  # values
  expect_identical(input$skim_variable, "nas")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$missing, 3L)
  expect_identical(input$complete, 0L)
  expect_identical(input$n, 3L)
  expect_equal(input$mean, NaN)
  expect_equal(input$sd, NaN)
  expect_NA(input$p0)
  expect_NA(input$p25)
  expect_NA(input$p50)
  expect_NA(input$p75)
  expect_NA(input$p100)
  skip_on_os("windows")
  expect_identical(input$hist, " ")
})

test_that("numeric skim is calculated correctly when x is all zeores or NAs.", {
  na_and_zeros <- as.numeric(c(NA, NA, NA, 0))
  x <- tibble::tibble(na_and_zeros)
  input <- skim(x)

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 4)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")

  # values
  expect_identical(input$skim_variable, "na_and_zeros")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$missing, 3L)
  expect_identical(input$complete, 1L)
  expect_identical(input$n, 4L)
  expect_equal(input$mean, 0)
  expect_NA(input$sd)
  expect_equal(input$p0, 0)
  expect_equal(input$p25, 0)
  expect_equal(input$p50, 0)
  expect_equal(input$p75, 0)
  expect_equal(input$p100, 0)
  skip_on_os("windows")
  expect_identical(input$hist, "▁▁▇▁▁")
})

test_that("Skimming with non-finite values works", {
  inf_vals <- c(Inf, 0, -Inf)
  x <- tibble::tibble(inf_vals)
  expect_warning(input <- skim(x))

  expect_identical(input$skim_variable, "inf_vals")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$missing, 0L)
  expect_identical(input$complete, 3L)
  expect_identical(input$n, 3L)
  expect_equal(input$mean, NaN)
  expect_equal(input$sd, NaN)
  expect_equal(input$p0, -Inf)
  expect_equal(input$p25, -Inf)
  expect_equal(input$p50, 0)
  expect_equal(input$p75, Inf)
  expect_equal(input$p100, Inf)
  skip_on_os("windows")
  expect_identical(input$hist, "▁▁▇▁▁")
})

<<<<<<< HEAD
test_that("skim returns expected response for factor vectors", {
  input <- skim(iris, Species)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n",
    "ordered", "n_unique", "top_counts"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 150)
  expect_equal(attrs$data_cols, 5)
  expect_equal(attrs$df_name, "`iris`")
  expect_equal(
    attrs$skimmers_used,
    list(factor = c(
      "missing", "complete", "n", "ordered",
      "n_unique", "top_counts"
    ))
  )

  # values
  expect_identical(input$skim_variable, "Species")
  expect_identical(input$skim_type, "factor")
  expect_identical(input$missing, 0L)
  expect_identical(input$complete, 150L)
  expect_identical(input$n, 150L)
  expect_false(input$ordered)
  expect_identical(input$top_counts, "set: 50, ver: 50, vir: 50")
})

test_that("skim handles factors when NAs are present", {
  dat <- iris
  dat$Species[15:18] <- NA
  input <- skim(dat, Species)
  expect_identical(input$missing, 4L)
  expect_identical(input$complete, 146L)
  expect_identical(input$top_counts, "ver: 50, vir: 50, set: 46")
})


test_that("skim returns expected response for character vectors", {
  dat <- c("AAAB", "ABc", "acb", NA, "", " ")
  x <- tibble::tibble(dat)
  input <- skim(x)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 10)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "empty", "n_unique", "whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 6)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c(
      "missing", "complete", "n", "min", "max",
      "empty", "n_unique", "whitespace"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "character")
  expect_identical(input$missing, 1L)
  expect_identical(input$complete, 5L)
  expect_identical(input$n, 6L)
  expect_identical(input$min, 0L)
  expect_identical(input$max, 4L)
  expect_identical(input$empty, 1L)
  expect_identical(input$n_unique, 5L)
  expect_identical(input$whitespace, 1L)
})

test_that("skim returns expected response for logical vectors", {
  dat <- dplyr::mutate(chickwts, log_col = stringr::str_detect(feed, "ea"))
  input <- skim(dat, log_col)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 7)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "mean",
    "count"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 3)
  expect_equal(attrs$df_name, "`dat`")
  expect_equal(
    attrs$skimmers_used,
    list(logical = c("missing", "complete", "n", "mean", "count"))
  )

  # values
  expect_identical(input$skim_variable, "log_col")
  expect_identical(input$skim_type, "logical")
  expect_identical(input$missing, 0L)
  expect_identical(input$complete, 71L)
  expect_identical(input$n, 71L)
  expect_equal(input$mean, 0.49, tolerance = .01)
  expect_identical(input$count, "FAL: 36, TRU: 35")
})

test_that("skim returns expected response for logical vectors with NA values", {
  dat <- dplyr::mutate(chickwts, log_col = stringr::str_detect(feed, "ea"))
  dat$log_col[15:18] <- NA
  input <- skim(dat, log_col)

  expect_identical(input$missing, 4L)
  expect_identical(input$complete, 67L)
  expect_identical(input$n, 71L)
  expect_equal(input$mean, 0.52, tolerance = .01)
  expect_identical(input$count, "TRU: 35, FAL: 32")
})

test_that("skim returns expected response for complex vectors", {
  dat <- dplyr::mutate(chickwts, test_complex = weight)
  dat$test_complex[1:2] <- dat$test_complex[1:2] + 2i
  dat$test_complex[15:18] <- NA
  input <- skim(dat, test_complex)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 5)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c("skim_type", "skim_variable", "missing", "complete", "n"))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 3)
  expect_equal(attrs$df_name, "`dat`")
  expect_equal(
    attrs$skimmers_used,
    list(complex = c("missing", "complete", "n"))
  )

  # values
  expect_identical(input$skim_variable, "test_complex")
  expect_identical(input$skim_type, "complex")
  expect_identical(input$missing, 4L)
  expect_identical(input$complete, 67L)
  expect_identical(input$n, 71L)
})

test_that("skim returns expected response for Date vectors", {
  dat <- seq(as.Date("2011-07-01"), by = 1, len = 9)
  x <- tibble::tibble(dat)
  input <- skim(x)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 9)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "median", "n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 9)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(Date = c(
      "missing", "complete", "n", "min", "max",
      "median", "n_unique"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "Date")
  expect_identical(input$missing, 0L)
  expect_identical(input$complete, 9L)
  expect_identical(input$n, 9L)
  expect_equal(input$min, as.Date("2011-07-01"))
  expect_equal(input$max, as.Date("2011-07-09"))
  expect_equal(input$median, as.Date("2011-07-05"))
  expect_identical(input$n_unique, 9L)
})

test_that("skim returns expected response for ts vectors", {
  input <- skim(freeny, y)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 15)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "start",
    "end", "frequency", "deltat", "mean", "sd", "min",
    "max", "median", "line_graph"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 39)
  expect_equal(attrs$data_cols, 5)
  expect_equal(attrs$df_name, "`freeny`")
  expect_equal(
    attrs$skimmers_used,
    list(ts = c(
      "missing", "complete", "n", "start", "end",
      "frequency", "deltat", "mean", "sd", "min",
      "max", "median", "line_graph"
    ))
  )

  # values
  expect_identical(input$skim_variable, "y")
  expect_identical(input$skim_type, "ts")
  expect_identical(input$missing, 0L)
  expect_identical(input$complete, 39L)
  expect_identical(input$n, 39L)
  expect_equal(input$start, 1962)
  expect_equal(input$end, 1971)
  expect_equal(input$frequency, 4)
  expect_equal(input$deltat, 0.25)
  expect_equal(input$mean, 9.31, tolerance = 0.001)
  expect_equal(input$sd, 0.316, tolerance = 0.001)
  expect_equal(input$min, 8.79, tolerance = 0.001)
  expect_equal(input$max, 9.79, tolerance = 0.001)
  expect_equal(input$median, 9.31, tolerance = 0.001)
  expect_identical(input$line_graph, "⣀⣀⠤⠤⠒⠒⠉⠉")
})

test_that("skim returns expected response for POSIXct vectors", {
  dat <- seq(as.POSIXct("2011-07-01 00:00:00", tz = "UTC"), by = 1, len = 10)
  dat[2] <- NA
  posix <- tibble::tibble(dat)
  input <- skim(posix)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 9)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "median", "n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 10)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`posix`")
  expect_equal(
    attrs$skimmers_used,
    list(POSIXct = c(
      "missing", "complete", "n", "min", "max",
      "median", "n_unique"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "POSIXct")
  expect_identical(input$missing, 1L)
  expect_identical(input$complete, 9L)
  expect_identical(input$n, 10L)
  expect_identical(input$min, as.POSIXct("2011-07-01 00:00:00", tz = "UTC"))
  expect_identical(input$max, as.POSIXct("2011-07-01 00:00:09", tz = "UTC"))
  expect_identical(input$median, as.POSIXct("2011-07-01 00:00:05", tz = "UTC"))
  expect_identical(input$n_unique, 9L)
})

test_that("skim returns expected response for list (not AsIs) vectors", {
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"),
    e = list("i", "j", "k", "l"),
    f = NA
  )
  list_frame <- tibble::tibble(dat)
  input <- skim(list_frame)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n",
    "n_unique", "min_length", "max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 6)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`list_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(list = c(
      "missing", "complete", "n", "n_unique",
      "min_length", "max_length"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "list")
  expect_identical(input$missing, 1L)
  expect_identical(input$complete, 5L)
  expect_identical(input$n, 6L)
  expect_identical(input$n_unique, 5L)
  expect_identical(input$min_length, 1L)
  expect_identical(input$max_length, 4L)
})

test_that("skim returns expected response for list with all NA's", {
  dat <- list(NA, NA, NA)
  list_frame <- tibble::tibble(dat)
  input <- skim(list_frame)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n",
    "n_unique", "min_length", "max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`list_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(list = c(
      "missing", "complete", "n", "n_unique",
      "min_length", "max_length"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "list")
  expect_identical(input$missing, 3L)
  expect_identical(input$complete, 0L)
  expect_identical(input$n, 3L)
  expect_identical(input$n_unique, 0L)
  expect_identical(input$min_length, 1L)
  expect_identical(input$max_length, 1L)
})

test_that("skim returns expected response for asis vectors", {
  dat <- I(list(5, 5:6, 5:10, NA))
  asis_frame <- tibble::tibble(dat)
  input <- skim(asis_frame)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n",
    "n_unique", "min_length", "max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 4)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`asis_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(AsIs = c(
      "missing", "complete", "n", "n_unique",
      "min_length", "max_length"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "AsIs")
  expect_identical(input$missing, 1L)
  expect_identical(input$complete, 3L)
  expect_identical(input$n, 4L)
  expect_identical(input$n_unique, 3L)
  expect_identical(input$min_length, 1L)
  expect_identical(input$max_length, 6L)
})

test_that("skim returns expected response for difftime vectors", {
  dat_datetime <- as.POSIXct("2011-07-01 00:00:00", tz = "UTC")
  dat <- difftime(dat_datetime, (dat_datetime - seq(-30, 60, 10)))
  dat[2] <- NA
  dt <- tibble::tibble(dat)
  input <- skim(dt)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 9)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "median", "n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 10)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`dt`")
  expect_equal(
    attrs$skimmers_used,
    list(difftime = c(
      "missing", "complete", "n", "min", "max",
      "median", "n_unique"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "difftime")
  expect_identical(input$missing, 1L)
  expect_identical(input$complete, 9L)
  expect_identical(input$n, 10L)
  expect_identical(input$min, as.difftime(-30, units = "secs"))
  expect_identical(input$max, as.difftime(60, units = "secs"))
  expect_identical(input$median, as.difftime(20, units = "secs"))
  expect_identical(input$n_unique, 9L)
})

test_that("skim handles objects with multiple classes", {
  dat <- seq(as.Date("2011-07-01"), by = 1, len = 10)
  dat[2] <- NA
  class(dat) <- c("strange_type", "Date")
  x <- tibble::tibble(dat)
  input <- skim(x)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 9)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "median", "n_unique"
  ))

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "Date")
})

test_that("skim treats unknown classes as character", {
  weight <- chickwts$weight
  x <- tibble::tibble(weight)
  class(x$weight) <- "strange"
  expect_warning(input <- skim(x))

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "empty", "n_unique", "whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c(
      "missing", "complete", "n", "min", "max",
      "empty", "n_unique", "whitespace"
    ))
  )

  # values
  expect_identical(input$skim_variable, "weight")
  expect_identical(input$skim_type, "character")
})

test_that("skim handles objects with two unknown classes", {
  feed <- chickwts$feed
  x <- tibble::tibble(feed)
  class(x$feed) <- c("strange", "stranger")
  expect_warning(input <- skim(x))

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n", "min",
    "max", "empty", "n_unique", "whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c(
      "missing", "complete", "n", "min", "max",
      "empty", "n_unique", "whitespace"
    ))
  )

  # values
  expect_identical(input$skim_variable, "feed")
  expect_identical(input$skim_type, "character")
})

test_that("Skimming a complete data frame works as expected", {
  input <- skim(chickwts)

  # dimensions
  expect_n_rows(input, 2)
  expect_n_columns(input, 16)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "missing", "complete", "n",
    "ordered", "n_unique", "top_counts",
    "mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 2)
  expect_equal(attrs$df_name, "`chickwts`")
  expect_equal(
    attrs$skimmers_used,
    list(
      numeric = c(
        "missing", "complete", "n", "mean", "sd", "p0",
        "p25", "p50", "p75", "p100", "hist"
      ),
      factor = c(
        "missing", "complete", "n", "ordered",
        "n_unique", "top_counts"
      )
    )
  )
})

test_that("skim handles a matrix as input", {
  expect_error(skim(volcano), NA)
})

test_that("skim handles a vector as input", {
  expect_error(skim(iris$Sepal.Length), NA)
})

test_that("You can use tidyselect negation", {
  input <- skim(chickwts, -weight)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # values
  expect_identical(input$skim_variable, "feed")
  expect_identical(input$skim_type, "factor")
})

test_that("Tidyselect helpers work as expected", {
  input <- skim(iris, starts_with("Sepal"))

  expect_n_rows(input, 2)
  expect_n_columns(input, 13)
  expect_identical(input$skim_variable, c("Sepal.Length", "Sepal.Width"))
})

  test_that("skim_tee prints only selected columns, but returns full object", {
    expect_output(skim_tee(iris, Species), "Species")
    expect_output(skim_tee(iris, Species), "^(?s)(?!.*Petal).*$", perl = TRUE)
    expect_output(
      skim_tee(iris, starts_with("Sepal")), "^(?s)(?!.*Petal).*$", perl = TRUE)
    expect_output(skim_tee(iris, -Species), "^(?s)(?!.*Species).*$", perl = TRUE)
    iris_grouped <- dplyr::group_by(iris, Species)
    expect_output(
      skim_tee(iris_grouped, Sepal.Length), "Species")
    expect_output(
      skim_tee(iris_grouped, Sepal.Length), "^(?s)(?!.*Petal).*$", perl = TRUE)
    expect_identical(skim_tee(iris, Species), iris)
  })

test_that("Skimming a grouped df works as expected", {
  grouped <- dplyr::group_by(mtcars, cyl, gear)
  input <- skim(grouped, mpg, disp)

  # dimensions
  expect_n_rows(input, 16)
  expect_n_columns(input, 15)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "cyl", "gear", "missing",
    "complete", "n", "mean", "sd", "p0", "p25", "p50",
    "p75", "p100", "hist"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 32)
  expect_equal(attrs$data_cols, 11)
  expect_equal(attrs$df_name, "`grouped`")
  expect_equal(as.character(attrs$groups), c("cyl", "gear"))
  expect_equal(
    attrs$skimmers_used,
    list(numeric = c(
      "missing", "complete", "n", "mean", "sd", "p0",
      "p25", "p50", "p75", "p100", "hist"
    ))
  )
})

test_that("Using skim_tee returns the object", {
  capture.output(skim_object <- skim_tee(chickwts))
  expect_identical(chickwts, skim_object)
})

test_that("Using skim_tee prints out the object", {
  expect_output(skim_tee(chickwts), "── Data Summary ────────────────────────")
  expect_output(skim_tee(chickwts), "                       Value")
  expect_output(skim_tee(chickwts), "Name                    .data", fixed = TRUE)
  expect_output(skim_tee(chickwts), "Number of rows             71")
  expect_output(skim_tee(chickwts), "Number of columns           2")
  expect_output(skim_tee(chickwts), "── Variable type: factor ")
})
