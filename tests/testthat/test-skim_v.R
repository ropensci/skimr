## Expected response for mtcars mpg ----------------------------------------

context("Skim a vector within a data frame")

correct <- tibble::tribble(
  ~type,          ~stat, ~level,     ~value,
  "numeric",  "missing",     NA,     0,
  "numeric", "complete",     NA,     32,
  "numeric",        "n",     NA,     32,
  "numeric",     "mean",     NA,     mean(mtcars$mpg),
  "numeric",       "sd",     NA,     sd(mtcars$mpg),
  "numeric",      "min",     NA,     min(mtcars$mpg),
  "numeric",   "median",     NA,     median(mtcars$mpg),
  "numeric", "quantile",     "25%",  quantile(mtcars$mpg, probs = .25, names = F),
  "numeric", "quantile",     "75%",  quantile(mtcars$mpg, probs = .75, names = F),
  "numeric",      "max",     NA,     max(mtcars$mpg),
  "numeric",     "hist","▂▅▇▇▇▃▁▁▂▂", 0
  )

test_that("skim_v returns expected response for numeric vectors", {
  input <- skim_v(mtcars$mpg)
  expect_identical(input, correct)
})


## Expected response for iris Species ----------------------------------------

context("Skim a factor within a data frame")

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",  "missing",      ".all",  0,
  "factor", "complete",      ".all",  150,
  "factor",        "n",      ".all",  150,
  "factor",    "count",    "setosa",  50,
  "factor",    "count","versicolor",  50,
  "factor",    "count", "virginica",  50,
  "factor",    "count",          NA,  0,
  "factor",   "n_unique",    ".all",  3)

test_that("skim_v returns expected response for factor vectors", {
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

## Expected response for iris Species ----------------------------------------

context("Skim a factor within a data frame that has NAs")

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",  "missing",      ".all",  4,
  "factor", "complete",      ".all",  146,
  "factor",        "n",      ".all",  150,
  "factor",    "count",    "setosa",  46,
  "factor",    "count","versicolor",  50,
  "factor",    "count", "virginica",  50,
  "factor",    "count",          NA,  4,
  "factor",   "n_unique",    ".all",  3)

test_that("skim_v returns expected response for factor vectors when NAs are present", {
  iris$Species[15:18] <- NA 
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

correct_pathological_numeric <- tibble::tribble(
  ~type,          ~stat, ~level,  ~value,
  "numeric",  "missing",     NA,  1,
  "numeric", "complete",     NA,  2,
  "numeric",        "n",     NA,  3,
  "numeric",     "mean",     NA,  0,
  "numeric",       "sd",     NA,  1.27381e+16,
  "numeric",      "min",     NA,  -(2^.Machine$double.digits),
  "numeric",   "median",     NA,  0,
  "numeric", "quantile",  "25%",  -4.5036e+15,
  "numeric",  "quantile", "75%",  4.5036e+15,
  "numeric",      "max",     NA,  +(2^.Machine$double.digits)
)


test_that("skim_v returns expected response for numeric vectors with NAs and extreme numbers", {
  input <- skim_v(c(+(2^.Machine$double.digits), NA, -(2^.Machine$double.digits)))
  expect_equal(input, correct_pathological_numeric, tolerance=1)
})

## Expected response for chr input ----------------------------------------

context("Skim a character within a data frame")

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",   "missing",     ".all",  2,
  "factor",  "complete",     ".all",  3,
  "factor",     "empty",     ".all",  1,
  "factor",         "n",     ".all",  5,
  "factor",       "min",     ".all",  0,
  "factor",       "max",     ".all",  4,
  "factor",  "n_unique",     ".all",  5)

test_that("skim_v returns expected response for chr vectors", {
  dat <- c("AAAB","ABc","acb",NA,"")
  input <- skim_v(dat)
  expect_identical(input, correct)
})

## Expected response for chickwt column string detecting 'ea' ---------------------------

context("Skim a logical within a data frame")

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,     ~value,
  "logi",  "missing",    ".all",          0,
  "logi", "complete",    ".all",         71,
  "logi",        "n",    ".all",         71,
  "logi",    "count",      TRUE,         35,
  "logi",    "count",     FALSE,         36,
  "logi",     "mean",    ".all",  0.4929577
  )

test_that("skim_v returns expected response for logical vectors", {
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea'))
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})

## Expected response for iris Species ----------------------------------------

context("Skim a logical within a data frame when NAs are present")

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,     ~value,
  "logi",  "missing",    ".all",          0,
  "logi", "complete",    ".all",         71,
  "logi",        "n",    ".all",         71,
  "logi",    "count",      TRUE,         35,
  "logi",    "count",     FALSE,         32,
  "logi",     "mean",    ".all",  0.5223881
)

test_that("skim_v returns expected response for logical vectors", {
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea')) 
  dat$log_col[15:18] <- NA 
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})
