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
  "numeric",       "sd", ".all",  sd(c(+(2^.Machine$double.digits), NA, -(2^.Machine$double.digits)), na.rm=T),
  "numeric",      "min", ".all",  -(2^.Machine$double.digits),
  "numeric",   "median", ".all",  0,
  "numeric", "quantile",  "25%",  quantile(c(+(2^.Machine$double.digits), NA, -(2^.Machine$double.digits)), probs = .25, names = F, na.rm=T),
  "numeric",  "quantile", "75%",  quantile(c(+(2^.Machine$double.digits), NA, -(2^.Machine$double.digits)), probs = .75, names = F, na.rm=T),
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
  "character",   "missing",     ".all",  1,
  "character",  "complete",     ".all",  4,
  "character",         "n",     ".all",  5,
  "character",       "min",     ".all",  0,
  "character",       "max",     ".all",  4,
  "character",     "empty",     ".all",  1,
  "character",  "n_unique",     ".all",  5)

test_that("skim_v returns expected response for chr vectors", {
  dat <- c("AAAB","ABc","acb",NA,"")
  input <- skim_v(dat)
  expect_identical(input, correct)
})

## Expected response for chickwt logical ---------------------------

context("Skim a logical within a data frame")

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,     ~value,
  "logical",  "missing",    ".all",          0,
  "logical", "complete",    ".all",         71,
  "logical",        "n",    ".all",         71,
  "logical",    "count",     FALSE,         36,
  "logical",    "count",      TRUE,         35,
  "logical",    "count",        NA,          0,
  "logical",     "mean",    ".all",       35/71
  )

test_that("skim_v returns expected response for logical vectors", {
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea'))
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})

## Expected response for chickwt logical with NA ----------------------------------------

context("Skim a logical within a data frame when NAs are present")

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,     ~value,
  "logical",  "missing",    ".all",          4,
  "logical", "complete",    ".all",         67,
  "logical",        "n",    ".all",         71,
  "logical",    "count",     FALSE,         32,
  "logical",    "count",      TRUE,         35,
  "logical",    "count",        NA,          4,
  "logical",     "mean",    ".all",      35/67
)


test_that("skim_v returns expected response for logical vectors", {
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea')) 
  dat$log_col[15:18] <- NA 
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})



## Expected response for iris Species with NA----------------------------------------

context("Skim a complex within a data frame when NAs are present")

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,     ~value,
  "complex",  "missing",    ".all",          3,
  "complex", "complete",    ".all",         68,
  "complex",        "n",    ".all",         71
)


test_that("skim_v returns expected response for complex vectors", {
  dat <-  chickwts %>% dplyr::mutate(test_complex = weight) 
  dat$test_complex[1:2] <- dat$test_complex[1:2] + 2i
  #dat$test_complex[3:5] <- NA
  input <- skim_v(dat$test_complex)
})
  
## Expected response for Date  ----------------------------------------

context("Skim a Date within a data frame")

correct <- tibble::tribble(
  ~type,       ~stat,       ~level,        ~value,
  "Date",  "missing",    ".all",               1,
  "Date", "complete",    ".all",               9,
  "Date",        "n",    ".all",              10,
  "Date",      "min",    ".all",           15156,
  "Date",      "max",    ".all",           15165,
  "Date",   "median",    ".all",           15161,
  "Date", "n_unique",    ".all",               10
)

test_that("skim_v returns expected response for Date vectors", {
  dat <- seq( as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  input <- skim_v(dat)
  expect_identical(input, correct)
})
