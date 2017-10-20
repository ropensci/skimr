context("Skim a data.frame")

test_that("Skimming a data frame works as expected", {
  input <- skim(chickwts)
  
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 23)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_identical(input$var, c(rep(c("weight"), each = 11), 
                                rep("feed", each = 12)))
  expect_identical(input$type, c(rep("numeric", each = 11), 
                                 rep("factor", each = 12)))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "mean", "sd", "min"))
  expect_identical(tail(input$stat), c(rep("top_counts", 5), rep("ordered", 1)))
  expect_identical(head(input$level), rep(".all", 6))
  expect_identical(tail(input$level),
                   c("linseed", "sunflower", "meatmeal", 
                     "horsebean", NA, ".all"  ))
  expect_equal(head(input$value), c(0, 71, 71, 261.3, 78.1, 108), tol = .01)
  expect_equal(tail(input$value), c( 12, 12, 11, 10, 0, 0))
  expect_identical(head(input$formatted),
                   c("0", "71", "71", "261.31", "78.07", "108"))
  expect_identical(tail(input$formatted),
                   c("lin: 12", "sun: 12", "mea: 11", "hor: 10",
                     "NA: 0", "FALSE"))
})

test_that("Using skim_tee returns the object", {
  capture.output(skim_object <- skim_tee(chickwts))
  expect_identical(chickwts, skim_object)
})

test_that("Using skim_tee prints out the object", {
  expect_output(skim_tee(chickwts), "Skim summary statistics")
  expect_output(skim_tee(chickwts), "n obs: 71")
  expect_output(skim_tee(chickwts), "n variables: 2")
  expect_output(skim_tee(chickwts), "Variable type: factor")
})

test_that("Skimming a grouped data frame works as expected", {
  input <- dplyr::group_by(mtcars, cyl, gear) %>% skim()
  
  # dimensions
  expect_length(input, 8)
  expect_equal(nrow(input), 792)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "grouped_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  
  # values
  expect_identical(input$cyl, rep(c(4, 6, 8), c(297, 297, 198)))
  expect_true(all(c(3, 4, 5) %in% input$gear))
  expect_equal(as.numeric(table(input$gear)), c(297, 198, 297))
  expect_identical(input$var, rep(c("mpg", "disp", "hp", "drat",
                                    "wt", "qsec", "vs", "am",
                                    "carb"), 8, each = 11))
  expect_identical(input$type, rep("numeric", 792))
  expect_identical(input$stat, rep(c("missing", "complete", "n", "mean", "sd",
                                     "min", "p25", "median", "p75", "max",
                                     "hist"), 72))
  expect_identical(input$level, rep(".all", 792))
  expect_identical(input$value[1:5], c(0, 1, 1, 21.5, NA))
  expect_identical(input$formatted[1:5], c("0", "1", "1", "21.5", "NA"))
})
