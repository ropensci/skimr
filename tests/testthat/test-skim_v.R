## Expected response for mtcars mpg ----------------------------------------

context("Skim a vector within a data frame")

correct <- tibble::tribble(
  ~type,          ~stat, ~level,     ~value,
  "numeric",  "missing",   ".all",     0,
  "numeric", "complete",   ".all",     32,
  "numeric",        "n",   ".all",     32,
  "numeric",     "mean",   ".all",     mean(mtcars$mpg),
  "numeric",       "sd",   ".all",     sd(mtcars$mpg),
  "numeric",      "min",   ".all",     min(mtcars$mpg),
  "numeric",   "median",   ".all",     median(mtcars$mpg),
  "numeric", "quantile",    "25%",  quantile(mtcars$mpg, probs = .25, names = F),
  "numeric", "quantile",    "75%",  quantile(mtcars$mpg, probs = .75, names = F),
  "numeric",      "max",   ".all",     max(mtcars$mpg),
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
  "numeric",  "missing", ".all",  1,
  "numeric", "complete", ".all",  2,
  "numeric",        "n", ".all",  3,
  "numeric",     "mean", ".all",  0,
  "numeric",       "sd", ".all",  1.27381e+16,
  "numeric",      "min", ".all",  -(2^.Machine$double.digits),
  "numeric",   "median", ".all",  0,
  "numeric", "quantile",  "25%",  -4.5036e+15,
  "numeric",  "quantile", "75%",  4.5036e+15,
  "numeric",      "max",  ".all",  +(2^.Machine$double.digits),
  "numeric",     "hist", "▇▁▁▁▁▁▁▁▁▇", 0
)


test_that("skim_v returns expected response for numeric vectors with NAs and extreme numbers", {
  input <- skim_v(c(+(2^.Machine$double.digits), NA, -(2^.Machine$double.digits)))
  expect_equal(input, correct_pathological_numeric, tolerance=1e-3)
})

## Expected response for chr input ----------------------------------------

context("Skim a character within a data frame")

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",   "missing",     ".all",  2,
  "factor",  "complete",     ".all",  3,
  "factor",         "n",     ".all",  5,
  "factor",       "min",     ".all",  0,
  "factor",       "max",     ".all",  4,
  "factor",     "empty",     ".all",  1,
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
  "logical",  "missing",    ".all",          0,
  "logical", "complete",    ".all",         71,
  "logical",        "n",    ".all",         71,
  "logical",    "count",     FALSE,         36,
  "logical",    "count",      TRUE,         35,
  "logical",    "count",        NA,          0,
  "logical",     "mean",    ".all",  0.4929577
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
  "logical",  "missing",    ".all",          4,
  "logical", "complete",    ".all",         67,
  "logical",        "n",    ".all",         71,
  "logical",    "count",     FALSE,         32,
  "logical",    "count",      TRUE,         35,
  "logical",    "count",        NA,          4,
  "logical",     "mean",    ".all",   0.5223881
)

test_that("skim_v returns expected response for logical vectors", {
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea')) 
  dat$log_col[15:18] <- NA 
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})
