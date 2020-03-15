context("Test skim function lists")

test_that("Only string scalars for types", {
  expect_error(sfl(mad, skim_type = c("1", "2")))
})

test_that("sfl's require at least one function", {
  skip("Skip to test option of allowing 0 length sfls.")
  expect_error(sfl(skim_type = "numeric"))
})

test_that("The interface for sfl's separates keep and drop functions", {
  input <- sfl(mad = mad, hist = NULL, skim_type = "test")
  expect_is(input, "skimr_function_list")
  expect_length(input, 2)
  expect_named(input, c("funs", "skim_type"))
  expect_identical(input$skim_type, "test")

  funs <- input$funs
  expect_is(funs, "list")
  expect_named(funs, c("mad", "hist"))
})

test_that("sfl's support dummy names", {
  input <- sfl(mean = ~ mean(., na.rm = TRUE), skim_type = "test")
  funs <- input$funs
  expect_equal(funs$mean, rlang::quo(mean(., na.rm = TRUE)))
})

test_that("sfl's automatically generate function names", {
  input <- sfl(mad, hist = NULL, ~ length(.)^2, "var")
  expect_named(input$funs, c("mad", "hist", "~length(.)^2", '"var"'))
})
