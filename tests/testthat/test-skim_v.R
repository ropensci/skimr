context("Skim a vector within a data frame")


# Expected response for mtcars mpg ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat, ~level,  ~value,
  "numeric",  "missing",     NA,  0,
  "numeric", "complete",     NA,  32,
  "numeric",        "n",     NA,  32,
  "numeric",     "mean",     NA,  mean(mtcars$mpg),
  "numeric",       "sd",     NA,  sd(mtcars$mpg),
  "numeric",      "min",     NA,  min(mtcars$mpg),
  "numeric",       "q1",     NA,  quantile(mtcars$mpg, probs = .25, names = F),
  "numeric",   "median",     NA,  median(mtcars$mpg),
  "numeric",       "q3",     NA,  quantile(mtcars$mpg, probs = .75, names = F),
  "numeric",      "max",     NA,  max(mtcars$mpg))

test_that("skim_v returns expected response for numeric vectors", {
  input <- skim_v(mtcars$mpg)
  expect_identical(input, correct)
})