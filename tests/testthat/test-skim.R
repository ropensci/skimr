context("Skim a data.frame")

test_that("Skimming a data frame works as expected", {
  input <- skim(chickwts)
  expect_is(input, "skim_df")
  expect_length(input, 6)
  expect_equal(nrow(input), 22)
  expect_identical(input$var, rep(c("weight", "feed"), each = 11))
  expect_identical(input$type, rep(c("numeric", "factor"), each = 11))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "mean", "sd", "min"))
  expect_identical(tail(input$stat), rep("top_counts", 6))
  expect_identical(head(input$level), rep(".all", 6))
  expect_identical(tail(input$level),
                   c("casein", "linseed", "sunflower", "meatmeal", "horsebean",
                     NA))
  expect_equal(head(input$value), c(0, 71, 71, 261.3, 78.1, 108), tol = .01)
  expect_equal(tail(input$value), c(12, 12, 12, 11, 10, 0))
  expect_identical(head(input$formatted),
                   c("0", "71", "71", "261.31", "78.07", "108.00"))
  expect_identical(tail(input$formatted),
                   c("cas: 12", "lin: 12", "sun: 12", "mea: 11", "hor: 10",
                     "NA: 0"))
})

test_that("Using skim_tee returns the object", {
  capture.output(skim_object <- skim_tee(chickwts))
  expect_identical(chickwts, skim_object)
})

test_that("Using skim_tee prints out the object", {
  expect_output(skim_tee(chickwts), "Skim summary statistics")
  expect_output(skim_tee(chickwts), "n obs: 22")
  expect_output(skim_tee(chickwts), "n variables: 6")
  expect_output(skim_tee(chickwts), "Variable type: factor")
})
