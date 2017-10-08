context("Change functions used by skim")
skim_with_defaults()

test_that("Skimmer list is updated correctly when changing functions", {
  funs <- list(median = median, mad = mad)
  skim_with(numeric = funs, append = FALSE)
  input <- show_skimmers()
  expect_identical(input$numeric, names(funs))

  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions can be changed for multiple types separately", {
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

test_that("Skimming functions can be changed for multiple types together", {
  skim_with_defaults()
  newfuns1 <- list(iqr = IQR,
                   q99 = purrr::partial(quantile, probs = .99))
  newfuns2 <- list(n2 = length)
  skim_with(numeric = newfuns1, factor = newfuns2, append = FALSE)
  input <- show_skimmers()
  
  expect_identical(input[["numeric"]], names(newfuns1))
  expect_identical(input[["factor"]], names(newfuns2))
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

test_that("Skimming functions without a class name return a message.", {
  skim_with_defaults()
  funs_no_class <- list( IQR)
  
  expect_error(skim_with(funs_no_class), "Please used named arguments")
})

test_that("Throw errors when arguments are incorrect", {
  skim_with_defaults()
  new_funs <- list(iqr = IQR, mad)
  expect_error(skim_with(numeric = new_funs, append = FALSE), "missing a name")
  
  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions can be changed.", {
  newfuns <- list(iqr = IQR,
                  quantile = purrr::partial(quantile, probs = .99))
  skim_with(numeric = newfuns, append = FALSE)
  input <- skim_v(iris$Sepal.Length)
  expect_identical(input$type, c("numeric", "numeric"))
  expect_identical(input$stat, c("iqr", "quantile"))
  expect_identical(input$level, c(".all", "99%"))
  expect_identical(input$value, c(IQR(iris$Sepal.Length), 7.7))
  expect_identical(input$formatted, c("1.3", "7.7"))
  
  # Restore defaults
  skim_with_defaults()
})

test_that("show_skimmers() has a correct list of default types", {
  correct <- c("numeric", "integer", "factor", "character", "logical",
               "complex", "date", "Date", "ts", "POSIXct")
  skimmers <- show_skimmers()
  input <- names(skimmers)
  identical(input, correct)
})

test_that("show_skimmers() lets you pick which type you want returned", {
  correct <- list(character = c("missing", "complete", "n", "min", "max",
                                "empty", "n_unique"))
  skimmers <- show_skimmers("character")
  expect_identical(correct, skimmers)
})

test_that("show_skimmers() lets you pick which many types you want returned", {
  correct <- list(numeric = c("missing", "complete", "n", "mean",  "sd", "min",
                              "p25", "median", "p75", "max", "hist"),
                  character = c("missing",  "complete", "n", "min", "max",
                                "empty", "n_unique"))
  skimmers <- show_skimmers(c("numeric", "character"))
  expect_identical(correct, skimmers)
})

test_that("show_skimmers() throws a warning when given an unassigned type", {
  expect_warning(skimmers <- show_skimmers("banana"), "aren't defined")
  expect_identical(skimmers, setNames(list(), character(0)))
})

test_that("show_skimmers() returns something if given an unassigned type", {
  expect_warning(skimmers <- show_skimmers(c("character", "banana")))
  correct <- list(character = c("missing", "complete", "n", "min", "max",
                                "empty", "n_unique"))
  expect_identical(skimmers, correct)
})

test_that("Skim functions can be removed by setting them to NULL", {
  skim_with(numeric = list(hist = NULL))
  correct <- list(numeric = c("missing", "complete", "n", "mean",  "sd", "min",
                              "p25", "median", "p75", "max"))
  input <- show_skimmers("numeric")
  expect_identical(correct, input)
  skim_with_defaults()
})

test_that("Skimming functions can be appended", {
  correct <- tibble::tribble(
    ~type,          ~stat,      ~level, ~value,                  ~formatted,
    "numeric",      "missing",  ".all", 0,                        "0",
    "numeric",      "complete", ".all", 150,                      "150",
    "numeric",      "n",        ".all", 150,                      "150",
    "numeric",      "mean",     ".all", mean(iris$Sepal.Length),  "5.84",
    "numeric",      "sd",       ".all", sd(iris$Sepal.Length),    "0.83",
    "numeric",      "min",      ".all", 4.3,                      "4.3",
    "numeric",      "p25",      ".all",  5.1,                     "5.1",
    "numeric",      "median",   ".all", 5.8,                      "5.8",
    "numeric",      "p75",      ".all",  6.4,                     "6.4",
    "numeric",      "max",      ".all", 7.9,                      "7.9",
    "numeric",      "hist",     ".all", NA,                "▂▇▅▇▆▅▂▂",
    "numeric",      "iqr",      ".all", IQR(iris$Sepal.Length),   "1.3",
    "numeric",      "mad",      ".all", mad(iris$Sepal.Length),   "1.04")
  funs <- list(iqr = IQR, mad = mad)
  skim_with(numeric = funs)
  input <- skim_v(iris$Sepal.Length)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions for new types can be added", {
  correct <- tibble::tribble(
    ~type,          ~stat,  ~level,   ~value,                 ~formatted,
    "new_type",      "iqr", ".all",   IQR(iris$Sepal.Length), "1.3",
    "new_type", "quantile",  "99%",   7.7,                    "7.7"
  )
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(new_type = funs)
  vector <- structure(iris$Sepal.Length, class = "new_type")
  input <- skim_v(vector)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Set skimming functions for multiple types", {
  correct <- tibble::tribble(
    ~type,          ~stat,      ~level,  ~value,                 ~formatted,
    "new_type",     "iqr",      ".all",  IQR(iris$Sepal.Length), "1.3",
    "new_type",     "quantile", "99%",   7.7,                    "7.7",
    "new_type",     "q2",       "99%",   7.7,                    "7.7")
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99),
    q2 = purrr::partial(quantile, probs = .99))
  skim_with(numeric = funs, new_type = funs, append = FALSE)
  vector <- structure(iris$Sepal.Length, class = "new_type")
  input <- skim_v(vector)
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Throw errors when arguments are incorrect", {
  funs <- list(iqr = IQR, mad)
  msg <- "A field is missing a name"
  expect_error(skim_with(numeric = funs, append = FALSE), msg)
  expect_error(skim_with(funs, append = FALSE), "named arguments")

  # Restore defaults
  skim_with_defaults()
})

test_that("Throw error when a function producing a list is used", {
  funs <- list(list_fun = as.list)
  collapsed <- paste("list_fun", collapse = ", ")
  msg <- paste0("Functions for class ", "numeric",
       " did not return atomic vectors: ", collapsed)
  skim_with(numeric = funs, append = FALSE)
  expect_error(skim_v(mtcars$mpg), msg)

  # Restore defaults
  skim_with_defaults()
})

test_that("Throw error when a function producing an unnamed vector is used", {
  test_fun <- function(x){
    unname(summary(lynx))
  }
  funs <- list(test_fun_unname = test_fun)
  collapsed <- paste("test_fun_unname", collapse = ", ")
  msg <- paste0("Names missing from the following functions:  ", collapsed)
  skim_with(ts = funs, append = FALSE)
  expect_error(skim_v(lynx), msg)
  
  # Restore defaults
  skim_with_defaults()
})
