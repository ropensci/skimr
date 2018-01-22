context("Skim a vector within a data frame")

test_that("skim_v returns expected response for numeric vectors", {
  quantiles <- quantile(mtcars$mpg, probs = c(.25, .75), names = FALSE)
  correct <- tibble::tribble(
    ~type,          ~stat, ~level,   ~value,              ~ formatted,
    "numeric",  "missing",   ".all", 0,                    "0",
    "numeric", "complete",   ".all", 32,                   "32",
    "numeric",        "n",   ".all", 32,                   "32",
    "numeric",     "mean",   ".all", mean(mtcars$mpg),     "20.09",
    "numeric",       "sd",   ".all", sd(mtcars$mpg),       "6.03", 
    "numeric",      "p0",   ".all", min(mtcars$mpg),      "10.4",
    "numeric",      "p25",   ".all", quantiles[1],         "15.43",
    "numeric",   "median",   ".all", median(mtcars$mpg),   "19.2",
    "numeric",      "p75",   ".all", quantiles[2],         "22.8",
    "numeric",      "p100",   ".all", max(mtcars$mpg),      "33.9",
    "numeric",     "hist",   ".all", NA,                   "▃▇▇▇▃▂▂▂")
  input <- skimr:::skim_v(mtcars$mpg)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for factor vectors", {
  correct <- tibble::tribble(
    ~type,           ~stat,      ~level,  ~value, ~formatted,
    "factor",    "missing",      ".all",  0L,      "0",
    "factor",   "complete",      ".all",  150L,    "150",
    "factor",          "n",      ".all",  150L,    "150",
    "factor",   "n_unique",      ".all",  3L,      "3",
    "factor", "top_counts",    "setosa",  50L,     "set: 50",
    "factor", "top_counts","versicolor",  50L,     "ver: 50",
    "factor", "top_counts", "virginica",  50L,     "vir: 50",
    "factor", "top_counts",          NA,  0L,      "NA: 0",
    "factor",    "ordered",      ".all",  0L,      "FALSE")
  input <- skimr:::skim_v(iris$Species)
  expect_identical(input, correct)
})

test_that("skim_v handles factors when NAs are present", {
  correct <- tibble::tribble(
    ~type,           ~stat,      ~level,  ~value,  ~formatted,
    "factor",    "missing",      ".all",  4L,       "4",
    "factor",   "complete",      ".all",  146L,     "146",
    "factor",          "n",      ".all",  150L,     "150",
    "factor",   "n_unique",      ".all",  3L,       "3",
    "factor", "top_counts","versicolor",  50L,      "ver: 50",
    "factor", "top_counts", "virginica",  50L,      "vir: 50",
    "factor", "top_counts",    "setosa",  46L,      "set: 46",
    "factor", "top_counts",          NA,  4L,       "NA: 4",
    "factor",    "ordered",      ".all",  0L,       "FALSE")
  dat <- iris
  dat$Species[15:18] <- NA 
  input <- skimr:::skim_v(dat$Species)
  expect_identical(input, correct)
})

test_that("skim_v handles numeric vectors with NAs and extreme numbers", {
  patho <- c((2 ^ .Machine$double.digits), NA,  -(2 ^ .Machine$double.digits))
  pqs <- quantile(patho, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
  correct_patho <- tibble::tribble(
    ~type,          ~stat, ~level,  ~value,                        ~formatted,
    "numeric",  "missing", ".all",  1,                              "1",
    "numeric", "complete", ".all",  2,                              "2",
    "numeric",        "n", ".all",  3,                              "3",
    "numeric",     "mean", ".all",  0,                              "0", 
    "numeric",       "sd", ".all",  sd(patho, na.rm = TRUE),        "1.3e+16",
    "numeric",      "p0", ".all",  -(2^.Machine$double.digits),    "-9e+15",
    "numeric",      "p25", ".all",  pqs[1],                     "-4.5e+15",
    "numeric",   "median", ".all",  0,                              "0",
    "numeric",      "p75", ".all",  pqs[2],                     "4.5e+15", 
    "numeric",      "p100", ".all",  +(2^.Machine$double.digits),   "9e+15",
    "numeric",     "hist", ".all", NA, "▇▁▁▁▁▁▁▇")
  input <- skimr:::skim_v(patho)
  expect_identical(input, correct_patho)
})

test_that("skim_v returns expected response for chr vectors", {
  correct <- tibble::tribble(
    ~type,          ~stat,     ~level,  ~value, ~formatted,
    "character",   "missing",     ".all",  1L, "1",
    "character",  "complete",     ".all",  4L, "4",
    "character",         "n",     ".all",  5L, "5",
    "character",       "min",     ".all",  0L, "0",
    "character",       "max",     ".all",  4L, "4",
    "character",     "empty",     ".all",  1L, "1",
    "character",  "n_unique",     ".all",  4L, "4")
  dat <- c("AAAB", "ABc", "acb", NA, "")
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for logical vectors", {
  correct <- tibble::tribble(
    ~type,       ~stat,    ~level,     ~value,    ~formatted,
    "logical",  "missing",    ".all",          0, "0",
    "logical", "complete",    ".all",         71, "71",
    "logical",        "n",    ".all",         71, "71",
    "logical",     "mean",    ".all",       35/71, "0.49",
    "logical",    "count",     FALSE,         36, "FAL: 36",
    "logical",    "count",      TRUE,         35, "TRU: 35",
    "logical",    "count",        NA,          0, "NA: 0")
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea'))
  input <- skimr:::skim_v(dat$log_col)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for logical vectors", {
  correct <- tibble::tribble(
    ~type,       ~stat,    ~level,     ~value, ~formatted,
    "logical",  "missing",    ".all",       4, "4",
    "logical", "complete",    ".all",      67, "67",
    "logical",        "n",    ".all",      71, "71",
    "logical",     "mean",    ".all",   35/67, "0.52",
    "logical",    "count",      TRUE,      35, "TRU: 35",
    "logical",    "count",     FALSE,      32, "FAL: 32",
    "logical",    "count",        NA,       4, "NA: 4")
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea')) 
  dat$log_col[15:18] <- NA 
  input <- skimr:::skim_v(dat$log_col)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for complex vectors", {
  correct <- tibble::tribble(
    ~type,       ~stat,    ~level,     ~value, ~formatted,
    "complex",  "missing",    ".all",       4L, "4",
    "complex", "complete",    ".all",      67L, "67",
    "complex",        "n",    ".all",      71L, "71")
  data("chickwts")
  dat <-  chickwts %>% dplyr::mutate(test_complex = weight) 
  dat$test_complex[1:2] <- dat$test_complex[1:2] + 2i
  dat$test_complex[15:18] <- NA 
  input <- skimr:::skim_v(dat$test_complex)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for Date vectors", {
  correct <- tibble::tribble(
    ~type,       ~stat,       ~level, ~value, ~formatted,
    "Date",  "missing",    ".all",         1, "1",
    "Date", "complete",    ".all",         9, "9",
    "Date",        "n",    ".all",        10, "10",
    "Date",      "min",    ".all",     15156, "2011-07-01",
    "Date",      "max",    ".all",     15165, "2011-07-10",
    "Date",   "median",    ".all",     15161, "2011-07-06",
    "Date", "n_unique",    ".all",         9, "9")
  dat <- seq(as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

# Note this test relies on the immediately prior correct definition
test_that("skim_v handles objects with multiple classes", {
  correct <- tibble::tribble(
    ~type,       ~stat,       ~level, ~value, ~formatted,
    "Date",  "missing",    ".all",         1, "1",
    "Date", "complete",    ".all",         9, "9",
    "Date",        "n",    ".all",        10, "10",
    "Date",      "min",    ".all",     15156, "2011-07-01",
    "Date",      "max",    ".all",     15165, "2011-07-10",
    "Date",   "median",    ".all",     15161, "2011-07-06",
    "Date", "n_unique",    ".all",         9, "9")
  dat <- seq(as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  class(dat) <- c("strange_type", "Date")
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

test_that("skim_v returns treats unknown classes as character", {
  correct <- tibble::tribble(
    ~type,        ~stat,        ~level, ~value, ~formatted,
    "character",  "missing",   ".all",       0L, "0",
    "character",  "complete",  ".all",      71L, "71",
    "character",  "n",         ".all",      71L, "71",
    "character",  "min",       ".all",       3L, "3",
    "character",  "max",       ".all",       3L, "3",
    "character",  "empty",     ".all",       0L, "0",
    "character",  "n_unique",  ".all",      66L, "66")
  weight <- chickwts$weight
  class(weight) <- "strange"
  expect_warning(input <- skimr:::skim_v(weight))
  expect_identical(input, correct)
})

test_that("skim_v handles objects with two unknown classes", {
  correct <- tibble::tribble(
    ~type,        ~stat,        ~level, ~value, ~formatted,
    "character",  "missing",   ".all",       0L, "0",
    "character",  "complete",  ".all",      71L, "71",
    "character",  "n",         ".all",      71L, "71",
    "character",  "min",       ".all",       1L, "1",
    "character",  "max",       ".all",       1L, "1",
    "character",  "empty",     ".all",       0L, "0",
    "character",  "n_unique",  ".all",      6L, "6")
  feed <- chickwts$feed
  class(feed) <- c("strange", "stranger")
  expect_warning(input <- skimr:::skim_v(feed))
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for ts vectors", {
  correct <- tibble::tribble(
    ~type,  ~stat,        ~level,   ~value,         ~formatted,
    "ts",   "missing",    ".all",   0,              "0",
    "ts",   "complete",   ".all",   39,             "39",
    "ts",   "n",          ".all",   39,             "39",
    "ts",   "start",      ".all",   1962,           "1962",
    "ts",   "end",        ".all",   1971,           "1971",
    "ts",   "frequency",  ".all",   4,              "4",
    "ts",   "deltat",     ".all",   0.25,           "0.25",
    "ts",   "mean",       ".all",   mean(freeny$y), "9.31",
    "ts",   "sd",         ".all",   sd(freeny$y),   "0.32",
    "ts",   "min",        ".all",   8.79137,        "8.79",
    "ts",   "max",        ".all",   9.79424,        "9.79",
    "ts",   "median",     ".all",   9.31378,        "9.31",
    "ts",   "line_graph", ".all", NA,               "⣀⣀⠤⠤⠒⠒⠉⠉")
  data(freeny)
  input <- skimr:::skim_v(freeny$y)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for POSIXct vectors", {
  correct <- tibble::tribble(
    ~type,       ~stat,       ~level, ~value,     ~formatted,
    "POSIXct",  "missing",    ".all", 1,          "1",
    "POSIXct", "complete",    ".all", 9,          "9",
    "POSIXct",        "n",    ".all", 10,         "10",
    "POSIXct",      "min",    ".all", 1309478400, "2011-07-01",
    "POSIXct",      "max",    ".all", 1309478409, "2011-07-01",
    "POSIXct",   "median",    ".all", 1309478405, "2011-07-01",
    "POSIXct", "n_unique",    ".all", 9,          "9") 
  dat <- seq(as.POSIXct("2011-07-01 00:00:00", tz = "UTC"), by=1, len=10)
  dat[2] <- NA
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for list (not AsIs) vectors", {
  correct <- tibble::tribble (
    ~type,    ~stat,           ~level,  ~value, ~formatted,
    "list",   "missing",        ".all",  1L,     "1",
    "list",   "complete",       ".all",  5L,     "5",
    "list",   "n",              ".all",  6L,     "6",
    "list",   "n_unique",       ".all",  5L,     "5",
    "list",   "min_length",     ".all",  1L,     "1",
    "list",   "median_length",  ".all",  3L,     "3",
    "list",   "max_length",     ".all",  4L,     "4")
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"),
    e = list("i", "j", "k", "l"),
    f = NA)
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

test_that("skimr:::skim_v returns expected response for list with all NA's", {
  correct <- tibble::tribble (
    ~type,    ~stat,           ~level,  ~value, ~formatted,
    "list",   "missing",       ".all",   3L,     "3",
    "list",   "complete",      ".all",   0L,     "0",
    "list",   "n",             ".all",   3L,     "3",
    "list",   "n_unique",      ".all",   0L,     "0",
    "list",   "min_length",    ".all",   NA,    "NA",
    "list",   "median_length", ".all",   NA,    "NA",
    "list",   "max_length",    ".all",   NA,    "NA")
  dat <- c(list(NA), list(NA), list(NA))
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for asis vectors", {
  correct <- tibble::tribble(
    ~type,  ~stat,        ~level,  ~value, ~formatted,
    "AsIs",  "missing",   ".all",  1L,      "1",
    "AsIs",  "complete",  ".all",  3L,      "3",
    "AsIs",  "n",         ".all",  4L,      "4",
    "AsIs",  "n_unique",  ".all",  3L,      "3",
    "AsIs",  "min_length",".all",  1L,      "1",
    "AsIs",  "max_length",".all",  6L,      "6")
  dat <- I(list(5, 5:6,5:10, NA))
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})

test_that("skim_v returns expected response for difftime vectors", {
  correct <- tibble::tribble(
    ~type,      ~stat,       ~level,  ~value, ~formatted,
    "difftime",  "missing",  ".all",    1,     "1",
    "difftime", "complete",  ".all",    9,     "9",
    "difftime",        "n",  ".all",   10,     "10",
    "difftime",      "min",  ".all",  -30,    "-30 secs",
    "difftime",      "max",  ".all",   60,     "60 secs",
    "difftime",   "median",  ".all",   20,     "20 secs",
    "difftime", "n_unique",  ".all",    9,     "9")
  dat_datetime <- as.POSIXct("2011-07-01 00:00:00", tz = "UTC")
  dat <- difftime(dat_datetime, (dat_datetime - seq(-30, 60, 10)))
  dat[2] <- NA
  input <- skimr:::skim_v(dat)
  expect_identical(input, correct)
})
  
test_that("Skim_v works when a function generates top_count 
          (which includes <NA> as a name)", {
  expected <- tibble::tribble(
    ~type,          ~stat,      ~level,  ~value,   ~formatted,
    "factor", "top_counts", "virginica",  50L,     "vir: 50",
    "factor", "top_counts",          NA,  0L,      "NA: 0"
  )
  expect_identical(skimr:::skim_v(iris$Species)[7:8,], expected)
})

test_that("numeric skim is calculated correctly when x is all NAs.", {
  x <- as.numeric(c(NA, NA, NA))
  input <- skim(x)
  correct <- tibble::tribble(
    ~variable, ~type,          ~stat, ~level,   ~value,              ~ formatted, 
    "x",        "numeric",  "missing",   ".all", 3,                    "3",
    "x",        "numeric", "complete",   ".all", 0,                    "0",
    "x",        "numeric",        "n",   ".all", 3,                    "3",
    "x",        "numeric",     "mean",   ".all", NaN,                 "NaN",
    "x",        "numeric",       "sd",   ".all", NA,                   "NA", 
    "x",        "numeric",      "p0",   ".all", NA,                    "NA",
    "x",        "numeric",      "p25",   ".all", NA,                   "NA",
    "x",        "numeric",   "median",   ".all", NA,                   "NA",
    "x",        "numeric",      "p75",   ".all", NA,                   "NA",
    "x",        "numeric",      "p100",  ".all", NA,                   "NA",
    "x",        "numeric",     "hist",   ".all", NA,                   " " )
  expect_identical(input[1:6], correct[1:6])
})
