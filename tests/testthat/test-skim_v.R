context("Skim a vector within a data frame")


# Expected response for mtcars mpg ----------------------------------------

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
  "numeric",      "max",     NA,     max(mtcars$mpg))

test_that("skim_v returns expected response for numeric vectors", {
  input <- skim_v(mtcars$mpg)
  expect_identical(input, correct)
})

context("Skim a factor within a data frame")


# Expected response for iris Species ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",  "missing",          NA,  0,
  "factor", "complete",          NA,  150,
  "factor",        "n",          NA,  150,
  "factor",    "count",    "setosa",  50,
  "factor",    "count","versicolor",  50,
  "factor",    "count", "virginica",  50,
  "factor",    "count",          NA,  0,
  "factor",   "n_unique",        NA,  3)

test_that("skim_v returns expected response for factor vectors", {
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

context("Skim a factor within a data frame that has NAs")


# Expected response for iris Species ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",  "missing",          NA,  4,
  "factor", "complete",          NA,  146,
  "factor",        "n",          NA,  150,
  "factor",    "count",    "setosa",  46,
  "factor",    "count","versicolor",  50,
  "factor",    "count", "virginica",  50,
  "factor",    "count",          NA,  4,
  "factor",   "n_unique",        NA,  3)

test_that("skim_v returns expected response for factor vectors when NAs are present", {
  iris$Species[15:18] <- NA 
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

context("Skim a character within a data frame")


# Expected response for chr input ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "factor",   "missing",          NA,  2,
  "factor",  "complete",          NA,  3,
  "factor",         "n",          NA,  5,
  "factor",       "min",          NA,  0,
  "factor",       "max",          NA,  4,
  "factor",  "n_unique",          NA,  5)

test_that("skim_v returns expected response for chr vectors", {
  dat <- c("AAAB","ABc","acb",NA,"")
  input <- skim_v(dat)
  expect_identical(input, correct)
})

context("Skim a logical within a data frame")


# Expected response for iris Species ----------------------------------------

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
  input <- skim_v(dat)
  expect_identical(input, correct)
})