test_that("skim returns expected response for numeric vectors", {
  input <- skim(mtcars, mpg)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 12)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
    list(
      numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist")
    )
  )

  # values
  expect_identical(unname(input$skim_variable), "mpg")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
  expect_equal(input$numeric.mean, 20.1, tolerance = 0.1)
  expect_equal(input$numeric.p0, 10.4, tolerance = 0.1)
  expect_equal(input$numeric.p25, 15.4, tolerance = 0.1)
  expect_equal(input$numeric.p50, 19.2, tolerance = 0.1)
  expect_equal(input$numeric.p75, 22.8, tolerance = 0.1)
  expect_equal(input$numeric.p100, 33.9, tolerance = 0.1)
  skip_if_not(l10n_info()$`UTF-8`)
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
  expect_identical(input$n_missing, 1L)
  expect_equal(input$complete_rate, .667, tolerance = .001)
  expect_equal(input$numeric.mean, 0)
  expect_equal(input$numeric.sd, 1.27e16, tolerance = 1e14)
  expect_equal(input$numeric.p0, -9.01e+15, tolerance = 1e14)
  expect_equal(input$numeric.p25, -4.5e+15, tolerance = 1e14)
  expect_equal(input$numeric.p50, 0)
  expect_equal(input$numeric.p75, 4.5e+15, tolerance = 1e14)
  expect_equal(input$numeric.p100, 9.01e+15, tolerance = 1e14)
  skip_if_not(l10n_info()$`UTF-8`)
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
  expect_identical(input$n_missing, 3L)
  expect_identical(input$complete_rate, 0)
  expect_equal(input$numeric.mean, NaN)
  expect_equal(input$numeric.sd, NaN)
  expect_NA(input$numeric.p0)
  expect_NA(input$numeric.p25)
  expect_NA(input$numeric.p50)
  expect_NA(input$numeric.p75)
  expect_NA(input$numeric.p100)
  skip_if_not(l10n_info()$`UTF-8`)
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
  expect_identical(input$n_missing, 3L)
  expect_equal(input$complete_rate, .25)
  expect_equal(input$numeric.mean, 0)
  expect_NA(input$numeric.sd)
  expect_equal(input$numeric.p0, 0)
  expect_equal(input$numeric.p25, 0)
  expect_equal(input$numeric.p50, 0)
  expect_equal(input$numeric.p75, 0)
  expect_equal(input$numeric.p100, 0)
  skip_if_not(l10n_info()$`UTF-8`)
  expect_identical(input$numeric.hist, "▁▁▇▁▁")
})

test_that("Skimming with non-finite values works", {
  inf_vals <- c(Inf, 0, -Inf)
  x <- tibble::tibble(inf_vals)
  expect_warning(input <- skim(x))

  expect_identical(input$skim_variable, "inf_vals")
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
  expect_equal(input$numeric.mean, NaN)
  expect_equal(input$numeric.sd, NaN)
  expect_equal(input$numeric.p0, -Inf)
  expect_equal(input$numeric.p25, -Inf)
  expect_equal(input$numeric.p50, 0)
  expect_equal(input$numeric.p75, Inf)
  expect_equal(input$numeric.p100, Inf)
  skip_if_not(l10n_info()$`UTF-8`)
  expect_identical(input$numeric.hist, "▁▁▇▁▁")
})

test_that("skim returns expected response for factor vectors", {
  input <- skim(iris, Species)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 7)
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "factor.ordered", "factor.n_unique", "factor.top_counts"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 150)
  expect_equal(attrs$data_cols, 5)
  expect_equal(attrs$df_name, "`iris`")
  expect_equal(
    attrs$skimmers_used,
    list(factor = c("ordered", "n_unique", "top_counts"))
  )

  # values
  expect_identical(unname(input$skim_variable), "Species")
  expect_identical(input$skim_type, "factor")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
  expect_false(input$factor.ordered)
  expect_identical(input$factor.top_counts, "set: 50, ver: 50, vir: 50")
})

test_that("skim handles factors when NAs are present", {
  dat <- iris
  dat$Species[15:18] <- NA
  input <- skim(dat, Species)
  expect_identical(input$n_missing, 4L)
  expect_equal(input$complete_rate, .973, tolerance = .001)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
    list(character = c("min", "max", "empty", "n_unique", "whitespace"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "character")
  expect_identical(input$n_missing, 1L)
  expect_equal(input$complete_rate, .833, tolerance = .001)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "logical.mean", "logical.count"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 3)
  expect_equal(attrs$df_name, "`dat`")
  expect_equal(
    attrs$skimmers_used,
    list(logical = c("mean", "count"))
  )

  # values
  expect_identical(unname(input$skim_variable), "log_col")
  expect_identical(input$skim_type, "logical")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
  expect_equal(input$logical.mean, 0.49, tolerance = .01)
  expect_identical(input$logical.count, "FAL: 36, TRU: 35")
})

test_that("skim returns expected response for logical vectors with NA values", {
  dat <- dplyr::mutate(chickwts, log_col = stringr::str_detect(feed, "ea"))
  dat$log_col[15:18] <- NA
  input <- skim(dat, log_col)

  expect_identical(input$n_missing, 4L)
  expect_equal(input$complete_rate, .944, tolerance = .001)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(
    input,
    c(
      "skim_type", "skim_variable", "n_missing", "complete_rate", "complex.mean"
    )
  )

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 3)
  expect_equal(attrs$df_name, "`dat`")
  expect_equal(
    attrs$skimmers_used,
    list(complex = "mean")
  )

  # values
  expect_identical(unname(input$skim_variable), "test_complex")
  expect_identical(input$skim_type, "complex")
  expect_identical(input$n_missing, 4L)
  expect_equal(input$complete_rate, .944, tolerance = .001)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "Date.min", "Date.max", "Date.median", "Date.n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 9)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`x`")
  expect_equal(
    attrs$skimmers_used,
    list(Date = c("min", "max", "median", "n_unique"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "Date")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
      "start", "end", "frequency", "deltat", "mean", "sd", "min",
      "max", "median", "line_graph"
    ))
  )

  # values
  expect_identical(unname(input$skim_variable), "y")
  expect_identical(input$skim_type, "ts")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
    list(POSIXct = c("min", "max", "median", "n_unique"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "POSIXct")
  expect_identical(input$n_missing, 1L)
  expect_equal(input$complete_rate, .9, tolerance = .001)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "list.n_unique", "list.min_length", "list.max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 6)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`list_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(list = c("n_unique", "min_length", "max_length"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "list")
  expect_identical(input$n_missing, 1L)
  expect_equal(input$complete_rate, .833, tolerance = .001)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "list.n_unique", "list.min_length", "list.max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`list_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(list = c("n_unique", "min_length", "max_length"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "list")
  expect_identical(input$n_missing, 3L)
  expect_equal(input$complete_rate, 0)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "AsIs.n_unique", "AsIs.min_length", "AsIs.max_length"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 4)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`asis_frame`")
  expect_equal(
    attrs$skimmers_used,
    list(AsIs = c("n_unique", "min_length", "max_length"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "AsIs")
  expect_identical(input$n_missing, 1L)
  expect_equal(input$complete_rate, .75)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
    list(difftime = c("min", "max", "median", "n_unique"))
  )

  # values
  expect_identical(input$skim_variable, "dat")
  expect_identical(input$skim_type, "difftime")
  expect_identical(input$n_missing, 1L)
  expect_equal(input$complete_rate, .9, tolerance = .001)
  expect_identical(input$difftime.min, as.difftime(-30, units = "secs"))
  expect_identical(input$difftime.max, as.difftime(60, units = "secs"))
  expect_identical(input$difftime.median, as.difftime(20, units = "secs"))
  expect_identical(input$difftime.n_unique, 9L)
})

test_that("skim returns expected response for lubridate Timespan vectors", {
  skip_if_not_installed("lubridate")
  dt <- tibble::tibble(x = lubridate::duration(1))
  input <- skim(dt)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 8)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "Timespan.min", "Timespan.max", "Timespan.median",
    "Timespan.n_unique"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 1)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`dt`")
  expect_equal(
    attrs$skimmers_used,
    list(Timespan = c("min", "max", "median", "n_unique"))
  )

  # values
  expect_identical(input$skim_variable, "x")
  expect_identical(input$skim_type, "Timespan")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1, tolerance = .001)
  expect_identical(input$Timespan.min, 1)
  expect_identical(input$Timespan.max, 1)
  expect_identical(input$Timespan.median, lubridate::duration(1))
  expect_identical(input$Timespan.n_unique, 1L)
})

test_that("skim handles objects containing haven_labelled vectors: double", {
  skip_if_not_installed("haven")
  dt <- tibble::tibble(x = haven::labelled(c(1, 2, 3), c(A = 1, B = 2, C = 3)))
  input <- skimr::skim(dt)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 12)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "numeric.mean", "numeric.sd", "numeric.p0", "numeric.p25",
    "numeric.p50", "numeric.p75", "numeric.p100", "numeric.hist"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`dt`")
  expect_equal(
    attrs$skimmers_used,
    list(
      numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist")
    )
  )

  # values
  expect_identical(input$skim_type, "numeric")
  expect_identical(input$skim_variable, "x")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1)
  expect_equal(input$numeric.mean, 2, tolerance = 0.1)
  expect_equal(input$numeric.sd, 1, tolerance = 0.1)
  expect_equal(input$numeric.p0, 1, tolerance = 0.1)
  expect_equal(input$numeric.p25, 1.5, tolerance = 0.1)
  expect_equal(input$numeric.p50, 2, tolerance = 0.1)
  expect_equal(input$numeric.p75, 2.5, tolerance = 0.1)
  expect_equal(input$numeric.p100, 3, tolerance = 0.1)
})

test_that("skim handles objects containing haven_labelled vectors: character", {
  skip_if_not_installed("haven")
  dt <- tibble::tibble(x = haven::labelled(
    c("a", "b", "c"),
    c(A = "a", B = "b", C = "c")
  ))
  input <- skimr::skim(dt)

  # dimensions
  expect_n_rows(input, 1)
  expect_n_columns(input, 9)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "character.min", "character.max", "character.empty",
    "character.n_unique", "character.whitespace"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 3)
  expect_equal(attrs$data_cols, 1)
  expect_equal(attrs$df_name, "`dt`")
  expect_equal(
    attrs$skimmers_used,
    list(character = c("min", "max", "empty", "n_unique", "whitespace"))
  )

  # values
  expect_identical(input$skim_variable, "x")
  expect_identical(input$skim_type, "character")
  expect_identical(input$n_missing, 0L)
  expect_equal(input$complete_rate, 1., tolerance = .001)
  expect_identical(input$character.min, 1L)
  expect_identical(input$character.max, 1L)
  expect_identical(input$character.empty, 0L)
  expect_identical(input$character.n_unique, 3L)
  expect_identical(input$character.whitespace, 0L)
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
    list(character = c("min", "max", "empty", "n_unique", "whitespace"))
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
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
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
    list(character = c("min", "max", "empty", "n_unique", "whitespace"))
  )

  # values
  expect_identical(input$skim_variable, "feed")
  expect_identical(input$skim_type, "character")
})

test_that("Skimming a complete data frame works as expected", {
  input <- skim(chickwts)

  # dimensions
  expect_n_rows(input, 2)
  expect_n_columns(input, 15)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "n_missing", "complete_rate",
    "factor.ordered", "factor.n_unique", "factor.top_counts",
    "numeric.mean", "numeric.sd", "numeric.p0", "numeric.p25", "numeric.p50",
    "numeric.p75", "numeric.p100", "numeric.hist"
  ))

  # attributes
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 71)
  expect_equal(attrs$data_cols, 2)
  expect_equal(attrs$df_name, "`chickwts`")
  expect_equal(attrs$base_skimmers, c("n_missing", "complete_rate"))
  expect_equal(
    attrs$skimmers_used,
    list(
      numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist"),
      factor = c("ordered", "n_unique", "top_counts")
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
  expect_n_columns(input, 12)

  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(
    input,
    c(
      "skim_type", "skim_variable", "n_missing", "complete_rate",
      "Date.min", "Date.max", "Date.median", "Date.n_unique",
      "POSIXct.min", "POSIXct.max", "POSIXct.median", "POSIXct.n_unique"
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
  expect_n_columns(input, 7)

  # values
  expect_identical(unname(input$skim_variable), "feed")
  expect_identical(input$skim_type, "factor")
})

test_that("Tidyselect helpers work as expected", {
  input <- skim(iris, starts_with("Sepal"))

  expect_n_rows(input, 2)
  expect_n_columns(input, 12)
  expect_identical(
    unname(input$skim_variable),
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that("Tidyselect predicates work as expected", {
  input <- skim(iris, where(is.numeric))

  expect_n_rows(input, 4)
  expect_n_columns(input, 12)
  expect_identical(
    unname(input$skim_variable),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})

test_that("Skimming a grouped df with selections works as expected", {
  grouped <- dplyr::group_by(mtcars, cyl, gear)
  input <- skim(grouped, mpg, disp)

  # dimensions
  expect_n_rows(input, 16)
  expect_n_columns(input, 14)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "cyl", "gear", "n_missing",
    "complete_rate", "numeric.mean", "numeric.sd", "numeric.p0",
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
    list(numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist"))
  )
})

test_that("Skimming a grouped df works when selecting exactly one variable", {
  grouped <- dplyr::group_by(mtcars, cyl, gear)
  input <- skim(grouped, mpg)

  # dimensions
  expect_n_rows(input, 8)
  expect_n_columns(input, 14)

  # classes
  expect_s3_class(input, "skim_df")
  expect_s3_class(input, "tbl_df")
  expect_s3_class(input, "tbl")
  expect_s3_class(input, "data.frame")
  expect_named(input, c(
    "skim_type", "skim_variable", "cyl", "gear", "n_missing",
    "complete_rate", "numeric.mean", "numeric.sd", "numeric.p0",
    "numeric.p25", "numeric.p50", "numeric.p75", "numeric.p100", "numeric.hist"
  ))
  expect_true(all(input$skim_variable == "mpg"))
})

test_that("Skimming without charts produces no histograms", {
  input <- skim_without_charts(iris, Sepal.Width)
  expect_null(iris$numeric.hist)
})

test_that("Skimming without charts produces no ts line charts", {
  input <- skim_without_charts(freeny, y)
  expect_null(iris$ts.line_graph)
})

test_that("Skimming succeeds when column names are similar", {
  input <- data.frame(
    x = 1:10,
    X = 11:20
  )
  skimmed <- skim(input)
  expect_s3_class(skimmed, "skim_df")
})


test_that("Skim inside a function returns the data name", {
  nested_skim <- function(df) {
    skim(df, .data_name = deparse(substitute(df)))
  }
  input <- nested_skim(iris)
  expect_equal(attr(input, "df_name"), "iris")
})

test_that("The data_name parameter can change the data_name attribute", {
  input <- skim(iris, .data_name = "Anderson")
  expect_equal(attr(input, "df_name"), "Anderson")
})
