context("Change functions used by skim")

correct <- tibble::tribble(
  ~type,          ~stat,  ~level,   ~value,
  "numeric",      "iqr",  ".all",   IQR(iris$Sepal.Length),
  "numeric", "quantile",   "99%",   7.7
)

test_that("Skimming functions can be changed for different types", {
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(numeric = funs, append = FALSE)
  input <- skim_v(iris$Sepal.Length)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,          ~stat,  ~level,   ~value,
  "new_type",      "iqr",  ".all",   IQR(iris$Sepal.Length),
  "new_type", "quantile",   "99%",   7.7
)

test_that("Skimming functions for new types can be added", {
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(new_type = funs)
  vector <- structure(iris$Sepal.Length, class = "new_type")
  input <- skim_v(vector)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,          ~stat,  ~level,   ~value,
  "new_type",      "iqr",  ".all",   IQR(iris$Sepal.Length),
  "new_type", "quantile",   "99%",   7.7,
  "new_type", "q2",   "99%",   7.7
)

test_that("Set multiple sets of skimming functions", {
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99),
    q2 = purrr::partial(quantile, probs = .99))
  skim_with(numeric = funs, new_type = funs, append = FALSE)
  vector <- structure(iris$Sepal.Length, class = "new_type")
  input <- skim_v(vector)
  expect_identical(input, correct)
})

skim_with_defaults()