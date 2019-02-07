context("Test skim function lists")

test_that("Only string scalars for types", {
  expect_error(sfl(mad, .type = c("1", "2")))
})

test_that("sfl's require at least one function", {
  expect_error(sfl(.type = "numeric"))
})

test_that("The interface for sfl's separates keep and drop functions", {
  input <- sfl(mad = mad, hist = NULL, .type = "test")
  expect_is(input, "skimr_function_list")
  expect_length(input, 2)
  expect_named(input, c("funs", "type"))
  expect_identical(input$type, "test")

  funs <- input$funs
  expect_is(funs, "list")
  expect_named(funs, c("mad", "hist"))
})

test_that("sfl's support dummy names", {
  input <- sfl(mean = ~ mean(., na.rm = TRUE), .type = "test")
  funs <- input$funs
  expect_equal(funs$mean, rlang::quo(mean(., na.rm = TRUE)))
})

test_that("sfl's automatically generate function names", {
  input <- sfl(mad, hist = NULL, ~ length(.)^2, "var")
  expect_named(input$funs, c("mad", "hist", "~length(.)^2", '"var"'))
})
