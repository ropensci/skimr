context("Skim a data frame")

test_that("skim returns expected response for numeric vectors", {
  input <- skim(mtcars, mpg)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 12)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "numeric.n_missing", "numeric.complete_rate",
    "numeric.mean", "numeric.sd", "numeric.p0", "numeric.p25",
    "numeric.p50", "numeric.p75", "numeric.p100", "numeric.hist"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 32)
  expect_equal(attrs$data_cols, 11)
  expect_equal(attrs$df_name, "`mtcars`")
  expect_equal(
    attrs$skimmers_used,
    list(numeric = c(
      "n_missing", "complete_rate", "mean", "sd", "p0",
      "p25", "p50", "p75", "p100", "hist"
    ))
  )

  # values
  expect_identical(input$skim_variable, "mpg")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$numeric.n_missing, 0L)
  expect_equal(input$numeric.complete_rate, 1)
  expect_equal(input$numeric.mean, 20.1, tolerance = 0.1)
  expect_equal(input$numeric.p0, 10.4, tolerance = 0.1)
  expect_equal(input$numeric.p25, 15.4, tolerance = 0.1)
  expect_equal(input$numeric.p50, 19.2, tolerance = 0.1)
  expect_equal(input$numeric.p75, 22.8, tolerance = 0.1)
  expect_equal(input$numeric.p100, 33.9, tolerance = 0.1)
  skip_on_os("windows")
  expect_identical(input$numeric.hist, "▃▇▅▁▂")
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
  expect_identical(input$numeric.n_missing, 1L)
  expect_equal(input$numeric.complete_rate, .667, tolerance = .001)
  expect_equal(input$numeric.mean, 0)
  expect_equal(input$numeric.sd, 1.27e16, tolerance = 1e14)
  expect_equal(input$numeric.p0, -9.01e+15, tolerance = 1e14)
  expect_equal(input$numeric.p25, -4.5e+15, tolerance = 1e14)
  expect_equal(input$numeric.p50, 0)
  expect_equal(input$numeric.p75, 4.5e+15, tolerance = 1e14)
  expect_equal(input$numeric.p100, 9.01e+15, tolerance = 1e14)
  skip_on_os("windows")
  expect_identical(input$numeric.hist, "▇▁▁▁▇")
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
  expect_identical(input$numeric.n_missing, 3L)
  expect_identical(input$numeric.complete_rate, 0)
  expect_equal(input$numeric.mean, NaN)
  expect_equal(input$numeric.sd, NaN)
  expect_NA(input$numeric.p0)
  expect_NA(input$numeric.p25)
  expect_NA(input$numeric.p50)
  expect_NA(input$numeric.p75)
  expect_NA(input$numeric.p100)
  skip_on_os("windows")
  expect_identical(input$numeric.hist, " ")
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
  expect_identical(input$numeric.n_missing, 3L)
  expect_equal(input$numeric.complete_rate, .25)
  expect_equal(input$numeric.mean, 0)
  expect_NA(input$numeric.sd)
  expect_equal(input$numeric.p0, 0)
  expect_equal(input$numeric.p25, 0)
  expect_equal(input$numeric.p50, 0)
  expect_equal(input$numeric.p75, 0)
  expect_equal(input$numeric.p100, 0)
  skip_on_os("windows")
  expect_identical(input$numeric.hist, "▁▁▇▁▁")
})

test_that("Skimming with non-finite values works", {
  inf_vals <- c(Inf, 0, -Inf)
  x <- tibble::tibble(inf_vals)
  expect_warning(input <- skim(x))

  expect_identical(input$skim_variable, "inf_vals")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$numeric.n_missing, 0L)
  expect_equal(input$numeric.complete_rate, 1)
  expect_equal(input$numeric.mean, NaN)
  expect_equal(input$numeric.sd, NaN)
  expect_equal(input$numeric.p0, -Inf)
  expect_equal(input$numeric.p25, -Inf)
  expect_equal(input$numeric.p50, 0)
  expect_equal(input$numeric.p75, Inf)
  expect_equal(input$numeric.p100, Inf)
  skip_on_os("windows")
  expect_identical(input$numeric.hist, "▁▁▇▁▁")
})

test_that("skim returns expected response for factor vectors", {
  input <- skim(iris, Species)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 7)
  expect_named(input, c(
    "skim_type", "skim_variable", "factor.n_missing", "factor.complete_rate",
    "factor.ordered", "factor.n_unique", "factor.top_counts"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 150)
  expect_equal(attrs$data_cols, 5)
  expect_equal(attrs$df_name, "`iris`")
  expect_equal(
    attrs$skimmers_used,
    list(factor = c(
      "n_missing", "complete_rate", "ordered",
      "n_unique", "top_counts"
    ))
  )

  # values
  expect_identical(input$skim_variable, "Species")
  expect_identical(input$skim_type, "factor")
  expect_identical(input$factor.n_missing, 0L)
  expect_equal(input$factor.complete_rate,  1)
  expect_false(input$factor.ordered)
  expect_identical(input$factor.top_counts, "set: 50, ver: 50, vir: 50")
})

test_that("skim handles factors when NAs are present", {
  dat <- iris
  dat$Species[15:18] <- NA
  input <- skim(dat, Species)
  expect_identical(input$factor.n_missing, 4L)
  expect_equal(input$factor.complete_rate, .973, tolerance = .001)
  expect_identical(input$factor.top_counts, "ver: 50, vir: 50, set: 46")
})


test_that("skim returns expected response for character vectors", {
  dat <- c("AAAB", "ABc", "acb", NA, "", " ")
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
    "skim_type", "skim_variable", "character.n_missing", "character.complete_rate",
    "character.min", "character.max", "character.empty",
    "character.n_unique", "character.whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 6)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c(
      "n_missing", "complete_rate", "min", "max",
      "empty", "n_unique", "whitespace"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "character")
  expect_identical(input$character.n_missing, 1L)
  expect_equal(input$character.complete_rate, .833, tolerance = .001)
  expect_identical(input$character.min, 0L)
  expect_identical(input$character.max, 4L)
  expect_identical(input$character.empty, 1L)
  expect_identical(input$character.n_unique, 5L)
  expect_identical(input$character.whitespace, 1L)
})

test_that("skim returns expected response for logical vectors", {
  dat <- dplyr::mutate(chickwts, log_col = stringr::str_detect(feed, "ea"))
  input <- skim(dat, log_col)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 6)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "logical.n_missing", "logical.complete_rate",
    "logical.mean", "logical.count"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 3)
  expect_equal(attrs$df_name, "`dat`")
  expect_equal(
    attrs$skimmers_used,
    list(logical = c("n_missing", "complete_rate", "mean", "count"))
  )

  # values
  expect_identical(input$skim_variable, "log_col")
  expect_identical(input$skim_type, "logical")
  expect_identical(input$logical.n_missing, 0L)
  expect_equal(input$logical.complete_rate, 1)
  expect_equal(input$logical.mean, 0.49, tolerance = .01)
  expect_identical(input$logical.count, "FAL: 36, TRU: 35")
})

test_that("skim returns expected response for logical vectors with NA values", {
  dat <- dplyr::mutate(chickwts, log_col = stringr::str_detect(feed, "ea"))
  dat$log_col[15:18] <- NA
  input <- skim(dat, log_col)

  expect_identical(input$logical.n_missing, 4L)
  expect_equal(input$logical.complete_rate, .944, tolerance = .001)
  expect_equal(input$logical.mean, 0.52, tolerance = .01)
  expect_identical(input$logical.count, "TRU: 35, FAL: 32")
})

test_that("skim returns expected response for complex vectors", {
  dat <- dplyr::mutate(chickwts, test_complex = weight)
  dat$test_complex[1:2] <- dat$test_complex[1:2] + 2i
  dat$test_complex[15:18] <- NA_complex_
  input <- skim(dat, test_complex)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 5)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
      "skim_type", "skim_variable", "complex.n_missing",
      "complex.complete_rate", "complex.mean"
    )
  )

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 3)
  expect_equal(attrs$df_name, "`dat`")
  expect_equal(
    attrs$skimmers_used,
    list(complex = c("n_missing", "complete_rate", "mean"))
  )

  # values
  expect_identical(input$skim_variable, "test_complex")
  expect_identical(input$skim_type, "complex")
  expect_identical(input$complex.n_missing, 4L)
  expect_equal(input$complex.complete_rate, .944, tolerance = .001)
  expect_equal(input$complex.mean, 265.2687 + 0.0597i, tolerance = .001)
})

test_that("skim returns expected response for Date vectors", {
  dat <- seq(as.Date("2011-07-01"), by = 1, len = 9)
  x <- tibble::tibble(dat)
  input <- skim(x)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "Date.n_missing", "Date.complete_rate",
    "Date.min", "Date.max", "Date.median", "Date.n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 9)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(Date = c(
      "n_missing", "complete_rate", "min", "max", "median", "n_unique"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "Date")
  expect_identical(input$Date.n_missing, 0L)
  expect_equal(input$Date.complete_rate, 1)
  expect_equal(input$Date.min, as.Date("2011-07-01"))
  expect_equal(input$Date.max, as.Date("2011-07-09"))
  expect_equal(input$Date.median, as.Date("2011-07-05"))
  expect_identical(input$Date.n_unique, 9L)
})

test_that("skim returns expected response for ts vectors", {
  input <- skim(freeny, y)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 14)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "ts.n_missing", "ts.complete_rate",
    "ts.start", "ts.end", "ts.frequency", "ts.deltat", "ts.mean", "ts.sd",
    "ts.min", "ts.max", "ts.median", "ts.line_graph"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 39)
  expect_equal(attrs$data_cols, 5)
  expect_equal(attrs$df_name, "`freeny`")
  expect_equal(
    attrs$skimmers_used,
    list(ts = c(
      "n_missing", "complete_rate", "start", "end",
      "frequency", "deltat", "mean", "sd", "min",
      "max", "median", "line_graph"
    ))
  )

  # values
  expect_identical(input$skim_variable, "y")
  expect_identical(input$skim_type, "ts")
  expect_identical(input$ts.n_missing, 0L)
  expect_equal(input$ts.complete_rate, 1)
  expect_equal(input$ts.start, 1962)
  expect_equal(input$ts.end, 1971)
  expect_equal(input$ts.frequency, 4)
  expect_equal(input$ts.deltat, 0.25)
  expect_equal(input$ts.mean, 9.31, tolerance = 0.001)
  expect_equal(input$ts.sd, 0.316, tolerance = 0.001)
  expect_equal(input$ts.min, 8.79, tolerance = 0.001)
  expect_equal(input$ts.max, 9.79, tolerance = 0.001)
  expect_equal(input$ts.median, 9.31, tolerance = 0.001)
  expect_identical(input$ts.line_graph, "⣀⣀⠤⠤⠒⠒⠉⠉")
})

test_that("skim returns expected response for POSIXct vectors", {
  dat <- seq(as.POSIXct("2011-07-01 00:00:00", tz = "UTC"), by = 1, len = 10)
  dat[2] <- NA
  posix <- tibble::tibble(dat)
  input <- skim(posix)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "POSIXct.n_missing", "POSIXct.complete_rate",
    "POSIXct.min", "POSIXct.max", "POSIXct.median",
    "POSIXct.n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 10)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`posix`")
  expect_equal(
    attrs$skimmers_used,
    list(POSIXct = c(
      "n_missing", "complete_rate", "min", "max",
      "median", "n_unique"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "POSIXct")
  expect_identical(input$POSIXct.n_missing, 1L)
  expect_equal(input$POSIXct.complete_rate, .9, tolerance = .001)
  expect_identical(
    input$POSIXct.min,
    as.POSIXct("2011-07-01 00:00:00", tz = "UTC")
  )
  expect_identical(
    input$POSIXct.max,
    as.POSIXct("2011-07-01 00:00:09", tz = "UTC")
  )
  expect_identical(
    input$POSIXct.median,
    as.POSIXct("2011-07-01 00:00:05", tz = "UTC")
  )
  expect_identical(input$POSIXct.n_unique, 9L)
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
  expect_n_columns(input, 7)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "list.n_missing", "list.complete_rate",
    "list.n_unique", "list.min_length", "list.max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 6)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`list_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(list = c(
      "n_missing", "complete_rate", "n_unique",
      "min_length", "max_length"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "list")
  expect_identical(input$list.n_missing, 1L)
  expect_equal(input$list.complete_rate, .833, tolerance = .001)
  expect_identical(input$list.n_unique, 5L)
  expect_identical(input$list.min_length, 1L)
  expect_identical(input$list.max_length, 4L)
})

test_that("skim returns expected response for list with all NA's", {
  dat <- list(NA, NA, NA)
  list_frame <- tibble::tibble(dat)
  input <- skim(list_frame)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 7)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "list.n_missing", "list.complete_rate",
    "list.n_unique", "list.min_length", "list.max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`list_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(list = c(
      "n_missing", "complete_rate", "n_unique",
      "min_length", "max_length"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "list")
  expect_identical(input$list.n_missing, 3L)
  expect_equal(input$list.complete_rate, 0)
  expect_identical(input$list.n_unique, 0L)
  expect_identical(input$list.min_length, 1L)
  expect_identical(input$list.max_length, 1L)
})

test_that("skim returns expected response for asis vectors", {
  dat <- I(list(5, 5:6, 5:10, NA))
  asis_frame <- tibble::tibble(dat)
  input <- skim(asis_frame)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 7)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "AsIs.n_missing", "AsIs.complete_rate",
    "AsIs.n_unique", "AsIs.min_length", "AsIs.max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 4)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`asis_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(AsIs = c(
      "n_missing", "complete_rate", "n_unique",
      "min_length", "max_length"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "AsIs")
  expect_identical(input$AsIs.n_missing, 1L)
  expect_equal(input$AsIs.complete_rate, .75)
  expect_identical(input$AsIs.n_unique, 3L)
  expect_identical(input$AsIs.min_length, 1L)
  expect_identical(input$AsIs.max_length, 6L)
})

test_that("skim returns expected response for difftime vectors", {
  dat_datetime <- as.POSIXct("2011-07-01 00:00:00", tz = "UTC")
  dat <- difftime(dat_datetime, (dat_datetime - seq(-30, 60, 10)))
  dat[2] <- NA
  dt <- tibble::tibble(dat)
  input <- skim(dt)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "difftime.n_missing", "difftime.complete_rate",
    "difftime.min", "difftime.max", "difftime.median",
    "difftime.n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 10)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`dt`")
  expect_equal(
    attrs$skimmers_used,
    list(difftime = c(
      "n_missing", "complete_rate", "min", "max",
      "median", "n_unique"
    ))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "difftime")
  expect_identical(input$difftime.n_missing, 1L)
  expect_equal(input$difftime.complete_rate, .9, tolerance = .001)
  expect_identical(input$difftime.min, as.difftime(-30, units = "secs"))
  expect_identical(input$difftime.max, as.difftime(60, units = "secs"))
  expect_identical(input$difftime.median, as.difftime(20, units = "secs"))
  expect_identical(input$difftime.n_unique, 9L)
})

test_that("skim handles objects with multiple classes", {
  dat <- seq(as.Date("2011-07-01"), by = 1, len = 10)
  dat[2] <- NA
  class(dat) <- c("strange_type", "Date")
  x <- tibble::tibble(dat)
  input <- skim(x)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "Date.n_missing", "Date.complete_rate",
    "Date.min", "Date.max", "Date.median", "Date.n_unique"
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
    "skim_type", "skim_variable", "character.n_missing", "character.complete_rate",
    "character.min", "character.max", "character.empty",
    "character.n_unique", "character.whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c(
      "n_missing", "complete_rate", "min", "max",
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
    "skim_type", "skim_variable", "character.n_missing", "character.complete_rate",
    "character.min", "character.max", "character.empty",
    "character.n_unique", "character.whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c(
      "n_missing", "complete_rate", "min", "max",
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
  expect_n_columns(input, 17)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "factor.n_missing", "factor.complete_rate",
    "factor.ordered", "factor.n_unique", "factor.top_counts",
    "numeric.n_missing", "numeric.complete_rate", "numeric.mean",
    "numeric.sd", "numeric.p0", "numeric.p25", "numeric.p50", "numeric.p75",
    "numeric.p100", "numeric.hist"
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
        "n_missing", "complete_rate", "mean", "sd", "p0",
        "p25", "p50", "p75", "p100", "hist"
      ),
      factor = c(
        "n_missing", "complete_rate", "ordered",
        "n_unique", "top_counts"
      )
    )
  )
})

test_that("successfully skim mixed data types with common skimmers", {
  df <- data.frame(
    Date = seq(as.Date("2011-07-01"), by = 1, len = 10),
    POSIXct = as.POSIXct("2011-07-01 00:00:00", tz = "UTC")
  )
  input <- skim(df)
  expect_n_rows(input, 2)
  expect_n_columns(input, 14)
  
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "Date.n_missing", "Date.complete_rate", 
    "Date.min", "Date.max", "Date.median", "Date.n_unique", 
    "POSIXct.n_missing", "POSIXct.complete_rate", "POSIXct.min", 
    "POSIXct.max", "POSIXct.median", "POSIXct.n_unique")
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
  expect_n_columns(input, 7)

  # values
  expect_identical(input$skim_variable, "feed")
  expect_identical(input$skim_type, "factor")
})

test_that("Tidyselect helpers work as expected", {
  input <- skim(iris, starts_with("Sepal"))

  expect_n_rows(input, 2)
  expect_n_columns(input, 12)
  expect_identical(input$skim_variable, c("Sepal.Length", "Sepal.Width"))
})

test_that("Skimming a grouped df works as expected", {
  grouped <- dplyr::group_by(mtcars, cyl, gear)
  input <- skim(grouped, mpg, disp)

  # dimensions
  expect_n_rows(input, 16)
  expect_n_columns(input, 14)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "cyl", "gear", "numeric.n_missing",
    "numeric.complete_rate", "numeric.mean", "numeric.sd", "numeric.p0",
    "numeric.p25", "numeric.p50", "numeric.p75", "numeric.p100", "numeric.hist"
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
      "n_missing", "complete_rate", "mean", "sd", "p0",
      "p25", "p50", "p75", "p100", "hist"
    ))
  )
})

test_that("Skimming a grouped df works as expected when selecting exactly 
          one variable", {
  grouped <- dplyr::group_by(mtcars, cyl, gear)
  input <- skim(grouped, mpg)

  # dimensions
  expect_n_rows(input, 8)
  expect_n_columns(input, 14)

  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "cyl", "gear", "numeric.n_missing",
    "numeric.complete_rate", "numeric.mean", "numeric.sd", "numeric.p0",
    "numeric.p25", "numeric.p50", "numeric.p75", "numeric.p100", "numeric.hist"
  ))
  expect_true(all(input$skim_variable == "mpg"))
})
