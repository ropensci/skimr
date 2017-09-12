context("Change functions used by skim")
skim_with_defaults()

test_that("Skimmer list is updated correctly when changing functions", {
  funs <- list(iqr = IQR,
               q99 = purrr::partial(quantile, probs = .99))
  correct <- names(funs)
  skim_with(numeric = funs, append = FALSE)
  input <- show_skimmers()["numeric"]
  expect_identical(unname(unlist(input)), names(funs))
  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions can be changed for multiple types", {
  skim_with_defaults()
  newfuns1 <- list(iqr = IQR,
                  q99 = purrr::partial(quantile, probs = .99))
  newfuns2 <- list(n2 = length)
  skim_with(numeric = newfuns1, append = FALSE)
  skim_with(factor = newfuns2, append = FALSE)
  input1 <- show_skimmers()[["numeric"]]
  input2 <- show_skimmers()[["factor"]]
  
  expect_identical(input1, names(newfuns1))
  expect_identical(input2, names(newfuns2))
  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions can be appended.", {
  skim_with_defaults()
  default_skimmers_numeric <- show_skimmers()[["numeric"]]
  correct <- c(default_skimmers_numeric, "iqr")
  funs <- list(iqr = IQR)
  skim_with(numeric = funs)
  input <-   show_skimmers()[["numeric"]]
  expect_identical(input, correct)  
  expect_identical(input, correct)
  # Restore defaults
  skim_with_defaults()
  
})

test_that("Setting a statistic to null removes it from the skimmers list.", {
  skim_with_defaults()
  numeric_skimmers <- show_skimmers()[["numeric"]]
  correct <- numeric_skimmers[!numeric_skimmers %in% "hist"]
  skim_with(numeric = list(hist = NULL))
  input <- show_skimmers()[["numeric"]]

  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions for new types can be added", {
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(new_type = funs)
  correct<- c("iqr", "quantile")
  expect_identical(show_skimmers()[["new_type"]], correct)
  # Restore defaults
  skim_with_defaults()
  
})

test_that("Set multiple sets of skimming functions", {
  skim_with_defaults()
  skimmers_default<-show_skimmers()
  correct <- c("iqr", "q" )
  
  funs <- list(iqr = IQR,
    q = purrr::partial(quantile, probs = .99))
  
  skim_with(numeric = funs, new_type = funs, append = FALSE)
  input1 <- show_skimmers()[["numeric"]]
  input2 <- show_skimmers()[["new_type"]]

  expect_identical(input1, correct)
  expect_identical(input2, correct)
  skim_with_defaults()
})

skim_with_defaults()
test_that("Skimming functions without a class name return a message.", {
  funs_no_class <- list( IQR)
  
  expect_error(skim_with(funs_no_class),
               "Please used named arguments as follows: <type> = <list of functions>"
               )
  #expect_error(skim_with(numeric = funs_no_name), "A function is missing a name within this type: numeric")
})

skim_with_defaults()

test_that("show_skimmers() has a correct list of functions for a type (default)", {
  skim_with_defaults()
  correct <- names(get_funs("numeric"))
  skimmers <- show_skimmers()
  input <- skimmers[["numeric"]]
  identical(input, correct)
})

test_that("show_skimmers() has a correct list of functions for a type using which", {
  skim_with_defaults()
  input <- show_skimmers(selected_classes = "numeric")
  skimmers <- show_skimmers()
  correct <- skimmers[["numeric"]]
  identical(input, correct)
  skim_with_defaults()
})

test_that("show_skimmers() has a correct list of types", {
  correct <- c("numeric",   "integer",  "factor" ,  "character", "logical",   "complex",
               "date",      "Date",      "ts", "POSIXct" )
  skimmers <- show_skimmers()
  input <- names(skimmers)
  identical(input, correct)
})


skim_with_defaults()