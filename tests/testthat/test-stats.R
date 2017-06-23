context("Use individual statistics functions on a vector")

# Expected response for freeny y inline_linegraph ----------------------------------------

correct <- 0
names(correct) <- "⣀⣀⣀⣀⣀⠤⠤⠤⠤⠔⠒⠒⠒⠒⠉⠉⠉⠉⠉⢁"

test_that("inline_linegraph returns expected response for a ts vector", {
  data("freeny")
  input <- inline_linegraph(freeny$y)
  expect_identical(input, correct)
})

# Expected response for AirPassengers y inline_linegraph ----------------------------------------

correct <- 0
names(correct) <- "⣀⣀⣀⣀⣀⣀⡠⢄⠤⠤⠢⠒⠒⠒⠒⠊⠉⠉"

test_that("inline_linegraph returns expected response for a ts vector of length > 30. Should be shortened.", {
  data("freeny")
  input <- inline_linegraph(AirPassengers)
  expect_identical(input, correct)
})

# Expected response for freeny y inline_linegraph where values are all NA--------------------------
correct <- 0
names(correct) <- ""
test_that("inline_linegraph returns expected response for a ts vector that is all NAs", {
  data("freeny")
  freeny$y <- NA
  input <- inline_linegraph(freeny$y)
  expect_identical(input, correct)
})

# Expected response for freeny ts_start --------------------------
correct <- 1962
test_that("ts_start returns expected response for a ts vector", {
  data("freeny")
  input <- ts_start(freeny$y)
  expect_identical(input, correct)
})

# Expected response for freeny ts_end --------------------------
correct <- 1971
test_that("ts_start returns expected response for a ts vector", {
  data("freeny")
  input <- ts_end(freeny$y)
  expect_identical(input, correct)
})

test_that("n_missing is calculated correctly.", {
  data<-c("a", "b", "c", NA)
    correct <- 1
    input <- n_missing(data)
    expect_identical(input, as.integer(correct))
})

test_that("n_complete is calculated correctly.", {
  data<-c("a", "b", "c", NA)
    correct <- as.integer(3)
    input <-n_complete(data)
    expect_identical(input, correct)
})

test_that("inline histogram is calculated correctly.", {
    correct <- 0
    names(correct) <-  "▂▇▅▇▆▆▅▂▂▂"
    input <- inline_hist(iris$Sepal.Length)
    expect_identical(input, correct)
})

test_that("inline histogram is calculated correctly with no data.", {
  correct <- 0
  names(correct) <-  ""
  all0s <- c(0, 0, 0, 0)
  input <- inline_hist(all0s)
  expect_identical(input, correct)
})

test_that("n_empty is calculated correctly.", {
  data<-c("a", "ab", "abc", "")
    correct <- as.integer(1)
    input <- n_empty(data)
    expect_identical(input, correct)
})

# min_char() includes empty strings.
test_that("min_char is calculated correctly.", {
  data<-c("a", "ab", "abc", "")
  correct <- as.integer(0)
  input <- min_char(data)
  expect_identical(input, correct)
})

# max_char() includes empty strings.
test_that("max_char is calculated correctly.", {
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
  dat <- list( list("a", "b", "c"), list("d", "b", "d"), list("e", "f", "g"), d <- list("h"), 
               e <- list("i", "j", "k", "l"), f <- NA)
  expect_identical(list_lengths_min(dat), 1L)
})

test_that("list_lengths_max is calculated correctly.", {
  dat <- list( list("a", "b", "c"), list("d", "b", "d"), list("e", "f", "g"), d <- list("h"), 
               e <- list("i", "j", "k", "l"), f <- NA)
  expect_identical( list_lengths_max(dat), 4L)
})

test_that("list_lengths_median is calculated correctly.", {
  dat <- list( list("a", "b", "c"), list("d", "b", "d"), list("e", "f", "g"), d <- list("h"), 
               e <- list("i", "j", "k", "l"), f <- NA)
  expect_identical( list_lengths_median(dat), 3L)
})

test_that("list_min_length is calculated correctly.", {
  dat <- I(list(5, 5:6,5:10, NA))
  expect_identical(list_min_length(dat), 1L)
})

test_that("list_max_length is calculated correctly.", {
  dat <- I(list(5, 5:6,5:10, NA))
  expect_identical(list_max_length(dat), 6L)
})

