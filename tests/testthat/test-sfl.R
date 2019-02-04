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
  expect_length(input, 3)
  expect_named(input, c("keep", "drop", "type"))
  expect_identical(input$drop, "hist")
  expect_identical(input$type, "test")

  keep <- input$keep
  expect_is(keep, "list")
  expect_named(keep, "mad")
})

test_that("sfl's support dummy names", {
  input <- sfl(mean = ~ mean(., na.rm = TRUE), .type = "test")
  keep <- input$keep
  expect_equal(keep$mean, rlang::quo(mean(., na.rm = TRUE)))
})
