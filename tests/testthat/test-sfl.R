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
  expect_is(keep, "fun_list")
  expect_true(attr(keep, "have_name"))
  expect_named(keep, "mad")
  expect_is(keep$mad, "quosure")
})

test_that("sfl's automatically generate names", {
  input <- sfl(mad, .type = "test")
  keep <- input$keep
  expect_is(keep, "fun_list")
  expect_false(attr(keep, "have_name"))
  expect_named(keep, "mad")
  expect_is(keep$mad, "quosure")
})

test_that("sfl's support dummy names", {
  input <- sfl(mean = mean(., na.rm = TRUE))
  keep <- input$keep
  expect_identical(keep$mean, rlang::quo(mean(., na.rm = TRUE)))
  res <- rlang::eval_tidy(keep$mean, list(. = c(NA, 1:10)))
  expect_equal(res, 5.5)
})
