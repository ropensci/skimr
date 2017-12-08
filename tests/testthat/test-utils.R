context("Utility functions")

test_that("skim_filter_type selects correctly",{
  data("chickwts")
  input <- skim(chickwts) %>% skim_filter_type("factor")
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 12)
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_identical(input$variable, rep("feed", each = 12))
  expect_identical(input$type, rep("factor", each = 12))
  expect_identical(input$stat, c("missing", "complete", "n", "n_unique", rep("top_counts", 7), rep("ordered", 1)))
  expect_identical(tail(input$level),
                   c("linseed", "sunflower", "meatmeal", 
                     "horsebean", NA, ".all"  ))
  expect_equal(input$value, c( 0, 71, 71, 6, 14, 12, 12, 12, 11, 10, 0, 0))
  expect_identical(tail(input$formatted),
                   c("lin: 12", "sun: 12", "mea: 11", "hor: 10",
                     "NA: 0", "FALSE"))
  
})

test_that("skim_wide_value returns correctly",{
  data("chickwts")
  input <- skim(chickwts) %>% skim_wide_value()
  # dimensions
  expect_length(input, 21)
  expect_equal(nrow(input), 2)
  # classes
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_equal(class(input$variable), "character")
  expect_equivalent(sapply(input[2:length(input)], class), rep("numeric", length(input) - 1))
  expect_equal(input$complete, c(71,71))
  expect_equivalent(input$hist, as.numeric(c(NA, NA)))
  expect_equal(input$max, c(NA, 423))
  expect_equal(input$ordered, c(0, NA))
})

test_that("skim_wide_formatted returns correctly",{
  data("chickwts")
  input <- skim(chickwts) %>% skim_wide_formatted()
  # dimensions
  expect_length(input, 21)
  expect_equal(nrow(input), 2)
  # classes
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_equivalent(sapply(input, class), rep("character", length(input)))
  expect_equal(input$complete, c("71","71"))
  expect_equal(input$max, c(NA, "423"))
  expect_equal(input$ordered, c("FALSE", NA))  
})
