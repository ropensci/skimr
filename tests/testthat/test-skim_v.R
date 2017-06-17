context("Skim a vector within a data frame")

# Expected response for freeny y ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat, ~level,   ~value,
  "numeric",  "missing",   ".all", 0,
  "numeric", "complete",   ".all", 32,
  "numeric",        "n",   ".all", 32,
  "numeric",     "mean",   ".all", mean(mtcars$mpg),
  "numeric",       "sd",   ".all", sd(mtcars$mpg),
  "numeric",      "min",   ".all", min(mtcars$mpg),
  "numeric",   "median",   ".all", median(mtcars$mpg),
  "numeric", "quantile",    "25%", quantile(mtcars$mpg, probs = .25, names = F),
  "numeric", "quantile",    "75%", quantile(mtcars$mpg, probs = .75, names = F),
  "numeric",      "max",   ".all", max(mtcars$mpg),
  "numeric",     "hist","▂▅▇▇▇▃▁▁▂▂", 0
  )

test_that("skim_v returns expected response for numeric vectors", {
  input <- skim_v(mtcars$mpg)
  expect_identical(input, correct)
})


## Expected response for iris Species ----------------------------------------

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

test_that("skim_v handles factors when NAs are present", {
  iris$Species[15:18] <- NA 
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

pathological <- c((2 ^ .Machine$double.digits), NA, 
    -(2 ^ .Machine$double.digits))
path_quantiles <- quantile(pathological, probs = c(.25, .75), na.rm = TRUE,
    names = FALSE)

correct_pathological_numeric <- tibble::tribble(
  ~type,          ~stat, ~level,  ~value,
  "numeric",  "missing", ".all",  1,
  "numeric", "complete", ".all",  2,
  "numeric",        "n", ".all",  3,
  "numeric",     "mean", ".all",  0,
  "numeric",       "sd", ".all",  sd(pathological, na.rm = TRUE),
  "numeric",      "min", ".all",  -(2^.Machine$double.digits),
  "numeric",   "median", ".all",  0,
  "numeric", "quantile",  "25%",  path_quantiles[1],
  "numeric",  "quantile", "75%",  path_quantiles[2],
  "numeric",      "max",  ".all",  +(2^.Machine$double.digits),
  "numeric",     "hist", "▇▁▁▁▁▁▁▁▁▇", 0.0
)

test_that("skim_v handles numeric vectors with NAs and extreme numbers", {
  input <- skim_v(pathological)
  expect_identical(input, correct_pathological_numeric)
})

## Expected response for chr input ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,  ~value,
  "character",   "missing",     ".all",  1,
  "character",  "complete",     ".all",  4,
  "character",         "n",     ".all",  5,
  "character",       "min",     ".all",  0,
  "character",       "max",     ".all",  4,
  "character",     "empty",     ".all",  1,
  "character",  "n_unique",     ".all",  4)

test_that("skim_v returns expected response for chr vectors", {
  dat <- c("AAAB","ABc","acb",NA,"")
  input <- skim_v(dat)
  expect_identical(input, correct)
})


# Expected response for chickwt logical ---------------------------

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
  data("chickwts")
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea'))
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})


# Expected response for chickwt logical with NA ---------------------------

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


# Expected response for iris Species with NA ------------------------------

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,     ~value,
  "complex",  "missing",    ".all",          4,
  "complex", "complete",    ".all",         67,
  "complex",        "n",    ".all",         71
)

test_that("skim_v returns expected response for complex vectors", {
  data("chickwts")
  dat <-  chickwts %>% dplyr::mutate(test_complex = weight) 
  dat$test_complex[1:2] <- dat$test_complex[1:2] + 2i
  dat$test_complex[15:18] <- NA 
  input <- skim_v(dat$test_complex)
  expect_identical(input, correct)
})

# Expected response for Date  ----------------------------------------

correct <- tibble::tribble(
  ~type,       ~stat,       ~level,        ~value,
  "Date",  "missing",    ".all",               1,
  "Date", "complete",    ".all",               9,
  "Date",        "n",    ".all",              10,
  "Date",      "min",    ".all",           15156,
  "Date",      "max",    ".all",           15165,
  "Date",   "median",    ".all",           15161,
  "Date", "n_unique",    ".all",               9
)

test_that("skim_v returns expected response for Date vectors", {
  dat <- seq(as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  input <- skim_v(dat)
  expect_identical(input, correct)
})

test_that("skim_v handles objects with multiple classes", {
  dat <- seq(as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  class(dat) <- c("strange_type", "Date")
  input <- skim_v(dat)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,       ~stat,       ~level,        ~value,
  "numeric",   "missing",   ".all",        0,
  "numeric",   "complete",  ".all",       71,
  "numeric",   "n",         ".all",       71,
  "numeric",   "mean",      ".all",       mean(as.numeric(chickwts$feed)),
  "numeric",   "sd",        ".all",       sd(as.numeric(chickwts$feed)),
  "numeric",   "min",       ".all",        1,
  "numeric",   "median",     ".all",       4,
  "numeric",   "quantile",   "25%",        2,
  "numeric",   "quantile",   "75%",        5,
  "numeric",   "max",        ".all",       6,
  "numeric",   "hist",  "▇▆▁▇▁▆▁▇▁▇",0
)

test_that("skim_v handles objects with two unknown classes", {
  data("chickwts")
  class(chickwts$feed) <- c("strange", "stranger")
  input<-skim_v(chickwts$feed)
})

# Expected response for ts  ---------------------------------------    
correct <- tibble::tribble(
  ~type,  ~stat,      ~level,   ~value,
  "ts",   "missing",   ".all",   0,
  "ts",   "complete",   ".all",   39,
  "ts",   "n",          ".all",   39,
  "ts",   "start",      ".all",   1962,
  "ts",   "end",        ".all",   1971,
  "ts",   "frequency",  ".all",   4,
  "ts",   "deltat",     ".all",   0.25,
  "ts",   "mean",       ".all",   mean(freeny$y),
  "ts",   "sd",         ".all",   sd(freeny$y),
  "ts",   "min",        ".all",   8.79137,
  "ts",   "max",        ".all",   9.79424,
  "ts",   "median",     ".all",   9.31378,
  "ts",   "line_graph", "⣀⣀⣀⣀⣀⠤⠤⠤⠤⠔⠒⠒⠒⠒⠉⠉⠉⠉⠉⢁",0
)

test_that("skim_v returns expected response for ts vectors", {
  data(freeny)
  input <- skim_v(freeny$y)
  expect_identical(input, correct)
})


correct <- tibble::tribble(
  ~type,       ~stat,       ~level,        ~value,
  "POSIXct",  "missing",    ".all",               1,
  "POSIXct", "complete",    ".all",               9,
  "POSIXct",        "n",    ".all",              10,
  "POSIXct",      "min",    ".all",         1309478400,
  "POSIXct",      "max",    ".all",         1309478409,
  "POSIXct",   "median",    ".all",         1309478405,
  "POSIXct", "n_unique",    ".all",               9
)

test_that("skim_v returns expected response for POSIXct vectors", {
  dat <- seq(as.POSIXct("2011-07-01 00:00:00", tz = "UTC"), by=1, len=10)
  dat[2] <- NA
  input <- skim_v(dat)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,  ~stat,        ~level,  ~value,
  "AsIs",  "missing",   ".all",  1,
  "AsIs",  "complete",  ".all",  3,
  "AsIs",  "n",         ".all",  4,
  "AsIs",  "n_unique",  ".all",  3,
  "AsIs",  "min_length",".all",  1,
  "AsIs",  "max_length",".all",  6
)

test_that("skim_v returns expected response for asis vectors", {
  dat <- I(list(5, 5:6,5:10, NA))
  input <- skim_v(dat)
  expect_identical(input, correct)
})
