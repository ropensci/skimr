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



