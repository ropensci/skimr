context("Changing functions used to skim")

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
  skim_with(reset = TRUE)
})