test_that("Skimmer list is updated correctly when changing functions", {
  funs <- sfl(median = median, mad = mad)
  new_skim <- skim_with(numeric = funs, append = FALSE)
  x <- tibble::tibble(rnorm(10))
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_identical(used, list(numeric = c("median", "mad")))
})

test_that("Skimming functions can be changed for multiple types", {
  newfuns1 <- sfl(iqr = IQR, q99 = ~ quantile(., probs = .99))
  newfuns2 <- sfl(n2 = length)
  new_skim <- skim_with(numeric = newfuns1, factor = newfuns2, append = FALSE)
  input <- new_skim(iris)
  used <- attr(input, "skimmers_used")
  expect_identical(used, list(numeric = c("iqr", "q99"), factor = "n2"))
})

test_that("Skimming functions can be changed with a list", {
  newfuns1 <- sfl(iqr = IQR, q99 = ~ quantile(., probs = .99))
  new_skim <- skim_with(list(numeric = newfuns1), append = FALSE)
  input <- new_skim(iris)
  used <- attr(input, "skimmers_used")
  expect_identical(used$numeric, c("iqr", "q99"))
})

test_that("Skimming functions can be changed with a list", {
  newfuns1 <- sfl(iqr = IQR, q99 = ~ quantile(., probs = .99))
  newfuns2 <- sfl(n2 = length)
  new_skim <- skim_with(
    list(numeric = newfuns1, factor = newfuns2),
    append = FALSE
  )
  input <- new_skim(iris)
  used <- attr(input, "skimmers_used")
  expect_identical(
    used, list(numeric = c("iqr", "q99"), factor = c("n2"))
  )
})

test_that("Skimming functions can be appended.", {
  funs <- sfl(iqr = IQR)
  new_skim <- skim_with(numeric = funs)
  x <- tibble::tibble(rnorm(10))
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_identical(
    used,
    list(
      numeric = c(
        "mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist", "iqr"
      )
    )
  )
})

test_that("Setting a statistic to null removes it from skimmers", {
  new_skim <- skim_with(numeric = sfl(hist = NULL))
  x <- tibble::tibble(rnorm(10))
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_identical(
    used, list(numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100"))
  )
})

test_that("Skimmers can be removed and added at the same time", {
  new_skim <- skim_with(numeric = sfl(hist = NULL, iqr = IQR))
  x <- tibble::tibble(rnorm(10))
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_identical(
    used,
    list(numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100", "iqr"))
  )
})

test_that("Skimming functions for new types can be added", {
  funs <- sfl(iqr = IQR, quantile = ~ quantile(., probs = .99))
  expect_message(new_skim <- skim_with(new_type = funs), "new_type")
  x <- tibble::tibble(x = rnorm(10))
  class(x$x) <- "new_type"
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_identical(used, list(new_type = c("iqr", "quantile")))
})

test_that("Set multiple sets of skimming functions", {
  funs <- sfl(iqr = IQR, quantile = ~ quantile(., probs = .99))
  expect_message(
    new_skim <- skim_with(numeric = funs, new_type = funs),
    "new_type"
  )
  x <- tibble::tibble(x = rnorm(10), y = rnorm(10))
  class(x$x) <- "new_type"
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_named(used, c("new_type", "numeric"))
  expect_identical(used$new_type, c("iqr", "quantile"))
  expect_identical(
    used$numeric,
    c(
      "mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist", "iqr", "quantile"
    )
  )
})


test_that("Set multiple sets of skimming functions, rlang", {
  funs <- sfl(iqr = IQR, quantile = ~ quantile(., probs = .99))
  expect_message(new_skim <- skim_with(!!!list(numeric = funs, new_type = funs),
    append = FALSE
  ))
  x <- tibble::tibble(x = rnorm(10), y = rnorm(10))
  class(x$x) <- "new_type"
  input <- new_skim(x)
  used <- attr(input, "skimmers_used")
  expect_named(used, c("new_type", "numeric"))
  expect_identical(used$new_type, c("iqr", "quantile"))
  expect_identical(used$numeric, c("iqr", "quantile"))
})

test_that("Skimming functions without a class return a message.", {
  funs_no_class <- sfl(IQR)
  expect_error(skim_with(funs_no_class), "arguments to be named.")
  expect_error(
    skim_with(funs_no_class, numeric = funs_no_class),
    "arguments to be named."
  )
})

test_that("An empty call to skim_with() returns the default skim()", {
  input <- skim_with()
  expect_identical(input(iris), skim(iris))
})

test_that("User-defined defaults require sfl's with class names", {
  with_mocked_bindings(
    get_skimmers = function(column) sfl(length),
    expect_error(skim(data.frame(1)), "Default skimming functions")
  )
})

test_that("Sfl's can be passed as an unquoted list", {
  my_skimmers <- list(numeric = sfl(mean), factor = sfl(length))
  my_skim <- skim_with(!!!my_skimmers, append = FALSE)
  input <- my_skim(iris)
  expect_named(
    input,
    c(
      "skim_type", "skim_variable", "n_missing", "complete_rate",
      "factor.length", "numeric.mean"
    )
  )
})

test_that("Doubles and integers are both 'numeric'", {
  df <- data.frame(int = 1:3, dbl = 1:3 + 0.5)
  my_skim <- skim_with(numeric = sfl(hist = NULL))
  input <- my_skim(df)
  
  expect_false("numeric.hist" %in% names(input))
  expect_equal(
    attr(input, "skimmers_used")$numeric,
    c("mean", "sd", "p0", "p25", "p50", "p75", "p100")
  )
})

test_that("Defining an integer sfl changes behavior", {
  df <- data.frame(int = 1:3, dbl = 1:3 + 0.5)
  my_skim <- expect_message(
    skim_with(
      numeric = sfl(hist = NULL), integer = sfl(int_mean = mean)
    )
  )
  input <- my_skim(df)

  expect_false("numeric.hist" %in% names(input))
  expect_true("integer.int_mean" %in% names(input))
  expect_identical(
    attr(input, "skimmers_used"),
    list(
      integer = c("int_mean"),
      numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100")
    )
  )
})

test_that("Base skimmers can be changed", {
  my_skim <- skim_with(base = sfl(length = length))
  skimmed <- my_skim(iris)
  expect_true("length" %in% names(skimmed))
  expect_equal(attr(skimmed, "base_skimmers"), "length")
})

test_that("Base skimmers require an sfl", {
  expect_error(skim_with(base = list(length = length)))
})

test_that("Base skimmers can be removed", {
  my_skim <- skim_with(base = NULL)
  skimmed <- my_skim(iris)
  used <- attr(skimmed, "skimmers_used")
  expect_false("base" %in% names(used))
})
