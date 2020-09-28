context("Use individual statistics functions on a vector")

test_that("inline_linegraph returns expected response for a ts vector", {
  input <- inline_linegraph(freeny$y)
  expect_identical(input, "⣀⣀⠤⠤⠒⠒⠉⠉")
})

test_that("inline_linegraph returns expected response for a long ts vector.", {
  input <- inline_linegraph(AirPassengers)
  expect_identical(input, "⣀⣀⣀⠔⠤⠊⠑⠊")
})

test_that("inline_linegraph returns expected response for an NA ts vector.", {
  input <- inline_linegraph(ts(c(NA, NA, NA, NA, NA, NA)))
  expect_identical(input, "⠀")
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
  data <- c("a", "b", "c", NA, " ")
  input <- n_missing(data)
  expect_identical(input, 1L)
})

test_that("n_complete is calculated correctly.", {
  data <- c("a", "b", "c", NA)
  input <- n_complete(data)
  expect_identical(input, 3L)
})

test_that("complete_rate is calculated correctly.", {
  data <- c("a", "b", "c", NA, " ")
  input <- complete_rate(data)
  expect_equal(input, .8, tolerance = .001)
})

test_that("n_whitespace is calculated correctly.", {
  data <- c("a", "b", "c", NA, " ", "  a", "   ", "   a")
  input <- n_whitespace(data)
  expect_identical(input, 2L)
})

test_that("inline histogram is calculated correctly.", {
  input <- inline_hist(iris$Sepal.Length)
  expect_identical(input, "▂▇▅▇▆▅▂▂")
})

test_that("inline histogram is calculated correctly when x is all zeros.", {
  input <- inline_hist(numeric(10))
  expect_identical(input, "▁▁▁▇▁▁▁▁")
})

test_that("inline histogram returns an empty string when x is length 0.", {
  input <- inline_hist(numeric(0))
  expect_identical(input, " ")
})

test_that("inline hist is calculated correctly when x is all zeores or NAs", {
  input <- inline_hist(as.numeric(c(NA, NA, NA, 0, 0)))
  expect_identical(input, "▁▁▁▇▁▁▁▁")
})

test_that("inline histogram is calculated correctly when x is all 1s.", {
  input <- inline_hist(c(1, 1, 1, 1, 1, 1))
  expect_identical(input, "▁▁▁▇▁▁▁▁")
})

test_that("inline histogram returns empty string when x is all NAs.", {
  input <- inline_hist(as.numeric(rep(NA, 10)))
  expect_identical(input, " ")
})

test_that("inline histogram is returns empty string when x is all NaN.", {
  input <- inline_hist(rep(NaN, 10))
  expect_identical(input, " ")
})

test_that("inline histogram is correct when x is evenly distributed.", {
  input <- inline_hist(c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_identical(input, "▇▇▇▇▇▇▇▇")
})

test_that("inline histogram is calculated correctly with NaN.", {
  input <- inline_hist(c(1, 2, 3, 3, 6, 6, 6, 8, NaN))
  expect_identical(input, "▂▂▅▁▁▇▁▂")
})

test_that("inline histogram is calculated correctly with NA.", {
  input <- inline_hist(c(1, 2, 3, 3, 6, 6, 6, 8, NA))
  expect_identical(input, "▂▂▅▁▁▇▁▂")
})

test_that("inline histogram is calculated correctly with Inf.", {
  expect_warning(input <-
    inline_hist(c(1, 2, 3, 3, 6, 6, 6, 8, Inf, -Inf)))
  expect_identical(input, "▂▂▅▁▁▇▁▂")
})

test_that("n_empty is calculated correctly.", {
  data <- c("a", "ab", "abc", "")
  correct <- as.integer(1)
  input <- n_empty(data)
  expect_identical(input, correct)
})

test_that("min_char is calculated correctly, including empty strings.", {
  data <- c("a", "ab", "abc", "")
  correct <- as.integer(0)
  input <- min_char(data)
  expect_identical(input, correct)
})

test_that("min_char with a multibyte character does not throw an error.", {
  data <- c("a", "ab", "abc", "Coleophora asteris M\x9fhl.")
  expect_error(min_char(data), NA)
})

test_that("min_char returns NA when there are only NA values.", {
  data <- as.character(c(NA, NA, NA, NA))
  input <- min_char(data)
  expect_equal(input, NA)
})

test_that("max_char is calculated correctly, including empty strings.", {
  data <- c("a", "ab", "abc", "")
  correct <- as.integer(3)
  input <- max_char(data)
  expect_identical(input, correct)
})

test_that("max_char with a multibyte character does not throw an error.", {
  data <- c("a", "ab", "abc", "Coleophora asteris M\x9fhl.")
  # correct <- as.integer(3)
  # input <- max_char(data)
  # expect_identical(input, correct)
  expect_error(max_char(data, NA))
})

test_that("max_char returns NA when there are only NA values.", {
  data <- as.character(c(NA, NA, NA, NA))
  input <- max_char(data)
  expect_equal(input, NA)
})

test_that("n_unique is calculated correctly.", {
  correct <- 4L
  data <- c("a", "ab", "abc", "")
  input <- n_unique(data)
  expect_identical(input, correct)
})

test_that("n_unique handles NA as expected.", {
  correct <- 4L
  data <- c("a", "ab", "abc", "", NA)
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
    f = NA
  )
  expect_identical(list_lengths_min(dat), 1L)
})

test_that("list_lengths_max is calculated correctly.", {
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"),
    e = list("i", "j", "k", "l"),
    f = NA
  )
  expect_identical(list_lengths_max(dat), 4L)
})

test_that("list_lengths_median is calculated correctly.", {
  dat <- list(
    list("a", "b", "c"),
    list("d", "b", "d"),
    list("e", "f", "g"),
    d = list("h"),
    e = list("i", "j", "k", "l"),
    f = NA
  )
  expect_identical(list_lengths_median(dat), 3L)
})

test_that("list_min_length is calculated correctly.", {
  dat <- I(list(5, 5:6, 5:10, NA))
  expect_identical(list_min_length(dat), 1L)
})

test_that("list_max_length is calculated correctly.", {
  dat <- I(list(5, 5:6, 5:10, NA))
  expect_identical(list_max_length(dat), 6L)
})

test_that("sorted count is calculated correctly.", {
  dat <- c("A", "A", "A", "B", "C", "C")
  expect_equal(sorted_count(dat)[1:3], c("A" = 3, "C" = 2, "B" = 1))
  expect_equal(names(sorted_count(dat)), c("A", "C", "B"))
})

test_that("top counts is calculated correctly", {
  dat <- c("A", "A", "A", "B", "C", "C")
  expect_equal(top_counts(dat), "A: 3, C: 2, B: 1")
})

test_that("sorted count is calculated correctly with a NA.", {
  # NA should be sorted as if it is a regular value
  dat <- c("A", "A", "A", "A", "B", NA, NA, "C", "C", "C")
  expect_equal(unname(sorted_count(dat)), c(4, 3, 1))
  expect_equal(names(sorted_count(dat)), c("A", "C", "B"))
})

test_that("top counts is calculated correctly with a NA", {
  dat <- c("A", "A", "A", "B", "C", "C")
  expect_equal(top_counts(dat), "A: 3, C: 2, B: 1")
})

test_that("sorted count is calculated correctly with \"\".", {
  # \"\" should be converted to \"empty \" and a warning issued.
  dat <- c("A", "A", "A", "A", "B", "", "", "C", "C", "C")
  dat <- as.factor(dat)
  expect_warning(expected <- sorted_count(dat))
  expect_equal(unname(expected), c(4, 3, 2, 1))
  expect_equal(names(expected), c("A", "C", "empty", "B"))
})
