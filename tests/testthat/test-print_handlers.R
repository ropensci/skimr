context("Handling of printing a skim_df objects of various types")

test_that("Number align does not change character vectors", {
  correct <- c("A", "B", "Z")
  input <- number_align(c("A", "B", "Z"))
  expect_identical(input,correct)
})

test_that("Number align does not change character vectors that include numbers", {
  correct <- c("A1", "B2.2", "3Z")
  input <- number_align(c("A1", "B2.2", "3Z"))
  expect_identical(input,correct)
})

test_that("Number align does not change integer strings that are all the same length", {
  correct <- c("1", "2", "3")
  input <- number_align(c("1", "2", "3"))
  expect_identical(input,correct)
})

test_that("Number align does not change numeric strings that are the same length", {
  correct <- c("1.1", "2.2", "3.3")
  input <- number_align(c("1.1", "2.2", "3.3"))
  expect_identical(input,correct)
})

test_that("Number align aligns a mix of integers and doubles", {
  correct <- c("1   ", "2.2 ", "3.33")
  input <- number_align(c("1", "2.2", "3.33"))
  expect_identical(input,correct)
})

