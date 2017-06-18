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



