context("Skim a data.frame")

test_that("Skimming a data frame works as expected", {
  input <- skim(chickwts)
  
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 22)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
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

test_that("Skimming a grouped data frame works as expected", {
  input <- dplyr::group_by(mtcars, cyl, gear) %>% skim()
  
  # dimensions
  expect_length(input, 8)
  expect_equal(nrow(input), 968)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "grouped_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  
  # values
  expect_identical(input$cyl, rep(c(4, 6, 8), c(363, 363, 242)))
  expect_true(all(c(3, 4, 5) %in% input$gear))
  expect_equal(as.numeric(table(input$gear)), c(363, 242, 363))
  expect_identical(input$var, rep(c("mpg", "cyl", "disp", "hp", "drat",
                                    "wt", "qsec", "vs", "am", "gear",
                                    "carb"), 8, each = 11))
  expect_identical(input$type, rep("numeric", 968))
  expect_identical(input$stat, rep(c("missing", "complete", "n", "mean", "sd",
                                     "min", "p25", "median", "p75", "max",
                                     "hist"), 88))
  expect_identical(input$level, rep(".all", 968))
  expect_identical(input$value[1:5], c(0, 1, 1, 21.5, NA))
  expect_identical(input$formatted[1:5], c("0", "1", "1", "21.50", "NA"))
})
