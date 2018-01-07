context("Change functions used by skim")
skim_with_defaults()

test_that("Skimmer list is updated correctly when changing functions", {
  funs <- list(median = median, mad = mad)
  skim_with(numeric = funs, append = FALSE)
  input <- show_skimmers()
  input_funs <- get_skimmers()
  expect_identical(input$numeric, names(funs))
  expect_identical(input_funs$numeric, funs)

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
  input1 <- show_skimmers("numeric")
  input2 <- show_skimmers("factor")
  
  expect_identical(input1$numeric, names(newfuns1))
  expect_identical(input2$factor, names(newfuns2))
  
  input1 <- get_skimmers("numeric")
  input2 <- get_skimmers("factor")
  
  expect_identical(input1$numeric, newfuns1)
  expect_identical(input2$factor, newfuns2)
  
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
  
  expect_identical(input$numeric, names(newfuns1))
  expect_identical(input$factor, names(newfuns2))
  
  input2 <- get_skimmers()
  
  expect_identical(input2$numeric, newfuns1)
  expect_identical(input2$factor, newfuns2)
  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions can be appended.", {
  skim_with_defaults()
  default_skimmers_numeric_names <- show_skimmers("numeric")
  default_skimmers_numeric <- get_skimmers("numeric")
  correct <- default_skimmers_numeric_names
  correct$numeric <- c(correct$numeric, "iqr")
  
  funs <- list(iqr = IQR)
  skim_with(numeric = funs)
  input <- show_skimmers("numeric")
  expect_identical(input, correct)
  
  correct <- default_skimmers_numeric
  correct$numeric <- c(correct$numeric, funs)
  input2 <- get_skimmers("numeric")
  # Restore defaults
  skim_with_defaults()
  
})

test_that("Setting a statistic to null removes it from the skimmers list.", {
  numeric_skimmers <- get_skimmers("numeric")
  numeric_skimmer_names <- show_skimmers("numeric")
  
  correct <- numeric_skimmer_names$numeric[-11]
  skim_with(numeric = list(hist = NULL))
  input <- show_skimmers("numeric")
  expect_identical(input$numeric, correct)
  
  correct <- numeric_skimmers
  correct$numeric$hist <- NULL
  input <- get_skimmers("numeric")
  expect_identical(input, correct)

  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions for new types can be added", {
  funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(new_type = funs)
  correct <- list(new_type = c("iqr", "quantile"))
  expect_identical(show_skimmers("new_type"), correct)
  expect_identical(get_skimmers("new_type"), list(new_type = funs))

  # Restore defaults
  skim_with_defaults()
})

test_that("Set multiple sets of skimming functions", {
  skimmers_default<-show_skimmers()
  correct <- c("iqr", "q" )

  funs <- list(iqr = IQR,
    q = purrr::partial(quantile, probs = .99))

  skim_with(numeric = funs, new_type = funs, append = FALSE)
  input <- show_skimmers(c("numeric", "new_type"))
  expect_identical(input$numeric, correct)
  expect_identical(input$new_type, correct)
  expect_identical(get_skimmers("new_type"), list(new_type = funs))
  skim_with_defaults()
})

test_that("Skimming functions without a class name return a message.", {
  funs_no_class <- list( IQR)
  expect_error(skim_with(funs_no_class), "Please used named arguments")
  skim_with_defaults()
})

test_that("Throw errors when arguments are incorrect", {
  new_funs <- list(iqr = IQR, mad)
  expect_error(skim_with(numeric = new_funs, append = FALSE), "missing a name")
  
  # Restore defaults
  skim_with_defaults()
})

test_that("Skimming functions can be changed.", {
  newfuns <- list(iqr = IQR,
                  quantile = purrr::partial(quantile, probs = .99))
  skim_with(numeric = newfuns, append = FALSE)
  input <- skimr:::skim_v(iris$Sepal.Length)
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
               "complex", "date", "Date", "ts", "POSIXct", "list",
               "AsIs", "difftime")
  skimmers <- show_skimmers()
  input <- names(skimmers)
  expect_identical(input, correct)
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
  expect_identical(skimmers, stats::setNames(list(), character(0)))
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
    ~type,          ~stat,      ~level,    ~value,              ~formatted,
    "factor",      "missing",    ".all",        0,                      "0",
    "factor",      "complete",   ".all",      150,                     "150",
    "factor",      "n",          ".all",      150,                     "150",
    "factor",      "n_unique",   ".all",        3,                       "3",
    "factor",      "top_counts", "setosa",     50,                 "set: 50",
    "factor",      "top_counts", "versicolor", 50,                 "ver: 50",
    "factor",      "top_counts", "virginica",  50,                 "vir: 50",
    "factor",      "top_counts",  NA,           0,                   "NA: 0",
    "factor",      "ordered",    ".all",    FALSE,                   "FALSE",
    "factor",      "isfactor",   ".all",     TRUE,                    "TRUE"
    )
  class(correct$value) <- "integer"
  funs <- list(isfactor = is.factor)
  skim_with(factor = funs)
  input <- skimr:::skim_v(iris$Species)
  expect_equal(input, correct)

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
  input <- skimr:::skim_v(vector)
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
  input <- skimr:::skim_v(vector)
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
  expect_error(skimr:::skim_v(mtcars$mpg), msg)

  # Restore defaults
  skim_with_defaults()
})

test_that("Throw error when a function producing an unnamed vector is used", {
  test_fun <- function(x){
    unname(summary(x))
  }
  funs <- list(test_fun_unname = test_fun)
  collapsed <- paste("test_fun_unname", collapse = ", ")
  msg <- paste0("Names missing from the following functions:  ", collapsed)
  skim_with(ts = funs, append = FALSE)
  expect_error(skimr:::skim_v(lynx), msg)
  
  # Restore defaults
  skim_with_defaults()
})

test_that("Errors are thrown when a vector with some only names is created", {
  test_fun2 <- function(x){
    r<-unname(summary(x))
    names(r)<- ""
    r
  }
  skim_format_defaults()
  funs <- list(test_fun_unname = test_fun2)
  collapsed <- paste("test_fun_unname", collapse = ", ")
  msg <- paste0("Names missing from the following functions:  ", collapsed)
  
  skim_with(ts = funs, append = FALSE)
  expect_error(skimr:::skim_v(lynx), msg)

  # Restore defaults
  skim_with_defaults()  
  
})

test_that("Errors are thrown when a vector with some empty names is created", {
  test_fun3 <- function(x){
    r<-unname(summary(x))
    names(r)<- ""
    names(r)[1]<- "name1"
    r
  }
  skim_format_defaults()
  funs <- list(test_fun_unname = test_fun3)
  collapsed <- paste("test_fun_unname", collapse = ", ")
  msg <- paste0("Names missing from the following functions:  ", collapsed)
  
  skim_with(ts = funs, append = FALSE)
  expect_error(skimr:::skim_v(lynx), msg)
  
  # Restore defaults
  skim_with_defaults()  
})
