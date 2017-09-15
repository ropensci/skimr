context("Change functions used by skim")
skim_with_defaults()

test_that("show_skimmers() has a correct list of functions for a type", {
  correct <- names(get_funs("numeric"))
  skimmers <- show_skimmers()
  input <- names(skimmers[["numeric"]])
  identical(input, correct)
})

test_that("show_skimmers() has a correct list of default types", {
  correct <- c("numeric", "integer", "factor", "character", "logical",
               "complex", "date", "Date", "ts", "POSIXct")
  skimmers <- show_skimmers()
  input <- names(skimmers)
  identical(input, correct)
})

test_that("show_skimmers() lets you pick which type you want returned", {
  correct <- list(character = c("missing", "complete", "n", "min", "max",
                                "empty", "n_unique"))
  skimmers <- show_skimmers("character")
  expect_identical(correct, skimmers)
})

test_that("show_skimmers() lets you pick which many types you want returned", {
  correct <- list(numeric = c("missing", "complete", "n", "mean",  "sd", "min",
                              "p25", "median", "p75", "max", "hist"),
                  character = c("missing",  "complete", "n", "min", "max",
                                "empty", "n_unique"))
  skimmers <- show_skimmers(c("numeric", "character"))
  expect_identical(correct, skimmers)
})

test_that("show_skimmers() throws a warning when given an unassigned type", {
  expect_warning(skimmers <- show_skimmers("banana"), "aren't defined")
  expect_identical(skimmers, setNames(list(), character(0)))
})

test_that("show_skimmers() returns something if given an unassigned type", {
  expect_warning(skimmers <- show_skimmers(c("character", "banana")))
  correct <- list(character = c("missing", "complete", "n", "min", "max",
                                "empty", "n_unique"))
  expect_identical(skimmers, correct)
})

test_that("Skimmer list is updated correctly when changing functions", {
  funs <- list(median = median, mad = mad)
  skim_with(numeric = funs, append = FALSE)
  input <- show_skimmers()
  expect_identical(input$numeric, names(funs))

  # Restore defaults
  skim_with_defaults()
})

test_that("Skim functions can be removed by setting them to NULL", {
  skim_with(numeric = list(hist = NULL))
  correct <- list(numeric = c("missing", "complete", "n", "mean",  "sd", "min",
                              "p25", "median", "p75", "max"))
  input <- show_skimmers("numeric")
  expect_identical(correct, input)
})

test_that("Skimming functions can be changed for different types", {
  correct <- tibble::tribble(
    ~type,     ~stat,      ~level,  ~value,                 ~formatted,
    "numeric", "iqr",      ".all",  IQR(iris$Sepal.Length), "1.30",
    "numeric", "quantile", "99%",   7.7,                    "7.70")
  newfuns <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(numeric = newfuns, append = FALSE)
  input <- skim_v(iris$Sepal.Length)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})


test_that("Skimming functions can be appended", {
  correct <- tibble::tribble(
    ~type,          ~stat,      ~level, ~value,                  ~formatted,
    "numeric",      "missing",  ".all", 0,                        "0",
    "numeric",      "complete", ".all", 150,                      "150",
    "numeric",      "n",        ".all", 150,                      "150",
    "numeric",      "mean",     ".all", mean(iris$Sepal.Length),  "5.84",
    "numeric",      "sd",       ".all", sd(iris$Sepal.Length),    "0.83",
    "numeric",      "min",      ".all", 4.3,                      "4.30",
    "numeric",      "p25",      ".all",  5.1,                     "5.10",
    "numeric",      "median",   ".all", 5.8,                      "5.80",
    "numeric",      "p75",      ".all",  6.4,                     "6.40",
    "numeric",      "max",      ".all", 7.9,                      "7.90",
    "numeric",      "hist",     ".all", NA,                "▂▇▅▇▆▆▅▂▂▂",
    "numeric",      "iqr",      ".all", IQR(iris$Sepal.Length),   "1.30",
    "numeric",      "mad",      ".all", mad(iris$Sepal.Length),   "1.04")
  funs <- list(iqr = IQR, mad = mad)
  skim_with(numeric = funs)
  input <- skim_v(iris$Sepal.Length)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

correct <- tibble::tribble(
  ~type,          ~stat,      ~level,       ~value,
  "numeric",      "missing",  ".all",       0,
  "numeric",      "complete", ".all",       150,
  "numeric",      "n",        ".all",       150,
  "numeric",      "mean",     ".all",       mean(iris$Sepal.Length),
  "numeric",      "sd",       ".all",       sd(iris$Sepal.Length),
  "numeric",      "min",      ".all",       4.3,
  "numeric",      "median",   ".all",       5.8,
  "numeric",      "quantile", "99%",        7.7,
  "numeric",      "max",      ".all",       7.9,
  "numeric",      "hist",     "▂▇▅▇▆▆▅▂▂▂", 0,
  "numeric",      "iqr",      ".all",       IQR(iris$Sepal.Length)
)

test_that("When append = FALSE, skimmers are replaced", {
  newfuns <- list(iqr = IQR,
                  quantile = purrr::partial(quantile, probs = .99))
  skim_with(numeric = newfuns, append = TRUE)
  input <- skim_v(iris$Sepal.Length)
})


test_that("Skimming functions for new types can be added", {
  correct <- tibble::tribble(
    ~type,          ~stat,  ~level,   ~value,                 ~formatted,
    "new_type",      "iqr", ".all",   IQR(iris$Sepal.Length), "1.30",
    "new_type", "quantile",  "99%",   7.7,                    "7.70"
  )
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(new_type = funs)
  vector <- structure(iris$Sepal.Length, class = "new_type")
  input <- skim_v(vector)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Set skimming functions for multiple types", {
  correct <- tibble::tribble(
    ~type,          ~stat,      ~level,  ~value,                 ~formatted,
    "new_type",     "iqr",      ".all",  IQR(iris$Sepal.Length), "1.30",
    "new_type",     "quantile", "99%",   7.7,                    "7.70",
    "new_type",     "q2",       "99%",   7.7,                    "7.70")
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99),
    q2 = purrr::partial(quantile, probs = .99))
  skim_with(numeric = funs, new_type = funs, append = FALSE)
  vector <- structure(iris$Sepal.Length, class = "new_type")
  input <- skim_v(vector)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Throw errors when arguments are incorrect", {
  funs <- list(iqr = IQR, mad)
  msg <- "A field is missing a name"
  expect_error(skim_with(numeric = funs, append = FALSE), msg)
  expect_error(skim_with(funs, append = FALSE), "named arguments")

  # Restore defaults
  skim_with_defaults()
})
