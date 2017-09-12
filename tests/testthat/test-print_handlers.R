context("Handling of printing a skim_df objects of various types")
#' @include print_handlers.R

test_that("Number align does not change character vectors", {
  correct <- c("A", "B", "Z")
  input <- number_align(c("A", "B", "Z"))
  expect_identical(input,correct)
})

test_that("Number align does not change character vectors that include numbers", {
  correct <- c("A1", "B2.2", "3Z")
  input <- number_align(c("A1", "B2.2", "3Z"))
  expect_identical(input,correct)
})

test_that("Number align does not change integer strings that are all the same length", {
  correct <- c("1", "2", "3")
  input <- number_align(c("1", "2", "3"))
  expect_identical(input,correct)
})

test_that("Number align does not change numeric strings that are the same length", {
  correct <- c("1.1", "2.2", "3.3")
  input <- number_align(c("1.1", "2.2", "3.3"))
  expect_identical(input,correct)
})

test_that("Number align aligns a mix of integers and doubles", {
  correct <- c("1   ", "2.2 ", "3.33")
  input <- number_align(c("1", "2.2", "3.33"))
  expect_identical(input,correct)
})

correct <- tibble::tribble(
  ~var,     ~complete, ~missing,   ~n, ~n_unique,                                    ~counts,
  "Species",  "150",   "0",       "150",  "3", "setosa:50 versicolor:50 virginica:50 NA:0"
  )
test_that("sk_print_factor returns correct results", {
  iris_species <- iris["Species"]
  x <- skim(iris_species)
  x_print <- sk_print_factor(x)

  expect_equal(correct, x_print)
})

correct <- tibble::tribble(
  ~var,    ~missing, ~complete, ~n,   ~mean,   ~sd,        ~min,  ~`quantile 25%`, ~`quantile 50%`, ~`quantile 75%`,~max, ~hist,
  "uptake", "0",        "84",   "84", " 27.2", " 10.8",     "7.7", " 17.9",           " 28.3",         " 37.1",       "45.5", "▅▇▆▅▂▅▇▆▇▅"
)
test_that("sk_print_default returns correct results", {
  co2_uptake <- CO2["uptake"]
  u <- skim(co2_uptake)
  u_print <- sk_print_default(u)
  expect_equal(correct, u_print)
})