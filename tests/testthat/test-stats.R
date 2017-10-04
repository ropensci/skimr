context("Use individual statistics functions on a vector")

test_that("inline_linegraph returns expected response for a ts vector", {
  input <- inline_linegraph(freeny$y)
  correct <- structure("⣀⣀⠤⠤⠒⠒⠉⠉", class = c("spark", "character"))
  expect_identical(input, correct)
})

test_that("inline_linegraph returns expected response for a long ts vector.", {
  input <- inline_linegraph(AirPassengers)
  correct <- structure("⣀⣀⣀⠔⠤⠊⠑⠊", class = c("spark", "character"))
  expect_identical(input, correct)
})

test_that("inline_linegraph returns expected response for an NA ts vector.", {
  input <- inline_linegraph(ts(c(NA, NA, NA, NA, NA, NA)))
  correct <- structure("⠀", class = c("spark", "character"))
  expect_identical(input, correct)
})

test_that("ts_start returns expected response for a ts vector", {
  input <- ts_start(freeny$y)
  expect_identical(input, 1962)
})

test_that("ts_start returns expected response for a ts vector", {
  input <- ts_end(freeny$y)
  expect_identical(input, 1971)
})

test_that("n_missing is calculated correctly.", {
  data <- c("a", "b", "c", NA)
  input <- n_missing(data)
  expect_identical(input, 1L)
})

test_that("n_complete is calculated correctly.", {
  data <- c("a", "b", "c", NA)
  input <- n_complete(data)
  expect_identical(input, 3L)
})

test_that("inline histogram is calculated correctly.", {
  input <- inline_hist(iris$Sepal.Length)
  correct <- structure("▂▇▅▇▆▅▂▂", class = c("spark", "character"))
  expect_identical(input, correct)
})

test_that("inline histogram is calculated correctly when x is all zeros.", {
  all0s <- c(0, 0, 0, 0)
  input <- inline_hist(all0s)
  correct <- structure("▁▁▁▇▁▁▁▁", class = c("spark", "character"))
  expect_identical(input, correct)
})

test_that("inline histogram is calculated correctly when x is all zeros.", {
  input <- inline_hist(numeric(0))
  correct <- structure(" ", class = "spark")
  expect_identical(input, correct)
})

test_that("n_empty is calculated correctly.", {
  data<-c("a", "ab", "abc", "")
    correct <- as.integer(1)
    input <- n_empty(data)
    expect_identical(input, correct)
})

test_that("min_char is calculated correctly, including empty strings.", {
  data<-c("a", "ab", "abc", "")
  correct <- as.integer(0)
  input <- min_char(data)
  expect_identical(input, correct)
})

test_that("max_char is calculated correctly, including empty strings.", {
  data<-c("a", "ab", "abc", "")
  correct <- as.integer(3)
  input <- max_char(data)
  expect_identical(input, correct)
})

test_that("n_unique is calculated correctly.", {
  correct <- 4L
  data<-c("a", "ab", "abc", "")
  input <- n_unique(data)
  expect_identical(input, correct)
})

test_that("n_unique handles NA as expected.", {
  correct <- 4L
  data<-c("a", "ab", "abc", "", NA)
  input <- n_unique(data)
  expect_identical(input, correct)
})

test_that("list_lengths_min is calculated correctly.", {
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"), 
    e = list("i", "j", "k", "l"),
    f = NA)
  expect_identical(list_lengths_min(dat), 1L)
})

test_that("list_lengths_max is calculated correctly.", {
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"), 
    e = list("i", "j", "k", "l"),
    f = NA)
  expect_identical(list_lengths_max(dat), 4L)
})

test_that("list_lengths_median is calculated correctly.", {
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"), 
    e = list("i", "j", "k", "l"),
    f = NA)
  expect_identical(list_lengths_median(dat), 3L)
})

test_that("list_min_length is calculated correctly.", {
  dat <- I(list(5, 5:6,5:10, NA))
  expect_identical(list_min_length(dat), 1L)
})

test_that("list_max_length is calculated correctly.", {
  dat <- I(list(5, 5:6,5:10, NA))
  expect_identical(list_max_length(dat), 6L)
})
