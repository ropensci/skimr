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
  expect_identical(input$variable, c(rep(c("weight"), each = 11), 
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
  expect_identical(input$variable, rep(c("mpg", "disp", "hp", "drat",
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

test_that("skim_to_wide works as expected.", {
  input <- skim_to_wide(iris)
  expect_length(input, 16)
  expect_equal(nrow(input), 5)
  expect_identical(input$type, c("factor", rep("numeric", each = 4)))
  expect_identical(input$variable, 
    c("Species", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))
  expect_identical(input$n, rep("150", each = 5))
  expect_identical(input$top_counts, c("set: 50, ver: 50, vir: 50, NA: 0", 
                                       NA, NA, NA, NA))
})

test_that("skim_to_list works as expected", {
  input <- skim_to_list(chickwts)
  expect_length(input, 2)
  expect_named(input, c("numeric", "factor"))
  expect_identical(class(input), "list")
  expect_identical(class(input[["numeric"]]), c("tbl", "tbl_df", "data.frame"))
  expect_equal(dim(input[["numeric"]]), c(1, 12))
  expect_identical(names(input[["numeric"]]), 
                   c("variable", "missing", "complete", "n", "mean",
                     "sd", "min", "p25", "median", "p75", "max", "hist" ))
})

test_that("skim_to_list works with grouped data", {
  xg <- dplyr::group_by(mtcars, cyl)
  input <- skim_to_list(xg)
  expect_length(input,1)
  expect_equal(dim(input[["numeric"]]), c(30,13))
})  

test_that("Skimming a vector works as expected", {
  input <- skim(lynx)
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 13)
  # classes
  expect_is(input, "skim_vector")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_identical(input$type, c(rep(c("ts"), each = 13)))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "start", "end", "frequency"))
  expect_identical(input$level, c(rep(c(".all"), each = 13)))
  expect_identical(input$variable, c(rep(c("lynx"), each = 13)))
  expect_equal(head(input$value), c(0, 114, 114, 1821, 1934, 1), tol = .01)
  expect_identical(head(input$formatted),
                   c("0", "114", "114", "1821", "1934", "1"))  
})

test_that("Skimming a column of a data frame works as expected", {
  input <- skim(chickwts$weight)
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 11)
  expect_is(input, "skim_vector")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_identical(input$variable, c(rep(c("chickwts$weight"), each = 11)))
  expect_identical(input$type, c(rep("numeric", each = 11)))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "mean", "sd", "min"))
  expect_identical(head(input$level), rep(".all", 6))
  expect_equal(head(input$value), c(0, 71, 71, 261.3, 78.1, 108), tol = .01)
  expect_identical(head(input$formatted),
                   c("0", "71", "71", "261.31", "78.07", "108"))
})

test_that("Skimming an object without a method returns the appropriate 
          messsage", {
  expect_message(skim(volcano), "No skim method exists for class matrix")
})

test_that("Skimming a data frame with selected columns works as expected", {
  input <- skim(chickwts, weight)
  
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 11)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  expect_identical(input$variable, rep("weight", 11))
  expect_identical(input$type, rep("numeric", 11))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "mean", "sd", "min"))
  expect_identical(head(input$level), rep(".all", 6))
  expect_equal(head(input$value), c(0, 71, 71, 261.3, 78.1, 108), tol = .01)
  expect_identical(head(input$formatted),
                   c("0", "71", "71", "261.31", "78.07", "108"))
})

test_that("You can use tidyselect negation", {
  input <- skim(chickwts, -weight)
  
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 12)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  
  # Content
  expect_identical(input$variable, rep("feed", 12))
  expect_identical(input$type, rep("factor", 12))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "n_unique", "top_counts",
                     "top_counts"))
  expect_identical(head(input$level), c(rep(".all", 4), "soybean", "casein"))
  expect_equal(head(input$value), c(0, 71, 71, 6, 14, 12))
  expect_identical(head(input$formatted),
                   c("0", "71", "71", "6", "soy: 14", "cas: 12"))
})

test_that("Tidyselect helpers work as expected", {
  input <- skim(iris, starts_with("Sepal"))
  
  # dimensions
  expect_length(input, 6)
  expect_equal(nrow(input), 22)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  
  # Content
  expect_identical(input$variable, rep(c("Sepal.Length", "Sepal.Width"),
                                       each = 11))
  expect_identical(input$type, rep("numeric", 22))
  expect_identical(head(input$stat),
                   c("missing", "complete", "n", "mean", "sd", "min"))
  expect_identical(head(input$level), rep(".all", 6))
  expect_equal(head(input$value), c(0, 150, 150, 5.84, 0.82, 4.3), 0.1)
  expect_identical(head(input$formatted),
                   c("0", "150", "150", "5.84", "0.83", "4.3"))
})

test_that("Skimming a grouped df works as expected selecting two columns", {
  input <- dplyr::group_by(mtcars, cyl, gear) %>% skim(mpg, disp)
  
  # dimensions
  expect_length(input, 8)
  expect_equal(nrow(input), 176)
  
  # classes
  expect_is(input, "skim_df")
  expect_is(input, "grouped_df")
  expect_is(input, "tbl_df")
  expect_is(input, "tbl")
  expect_is(input, "data.frame")
  
  # values
  expect_identical(input$cyl, rep(c(4, 6, 8), c(66, 66, 44)))
  expect_true(all(c(3, 4, 5) %in% input$gear))
  expect_equal(as.numeric(table(input$gear)), c(66, 44, 66))
  expect_identical(input$variable, rep(c("mpg", "disp"), 8, each = 11))
  expect_identical(input$type, rep("numeric", 176))
  expect_identical(input$stat, rep(c("missing", "complete", "n", "mean", "sd",
                                     "min", "p25", "median", "p75", "max",
                                     "hist"), 16))
  expect_identical(input$level, rep(".all", 176))
  expect_identical(input$value[1:5], c(0, 1, 1, 21.5, NA))
  expect_identical(input$formatted[1:5], c("0", "1", "1", "21.5", "NA"))
})

