context("Get skimmers")

test_that("get_default_skimmers() has a correct list of defaults", {
  defaults <- get_default_skimmers()
  expect_named(defaults, c("AsIs", "character", "complex", "Date", "difftime",
                           "factor",  "integer", "list", "logical", "numeric",
                           "POSIXct", "ts"))
  expect_identical(defaults$AsIs, c("missing", "complete", "n", "n_unique",
                                    "min_length", "max_length"))
  expect_identical(defaults$character, c("missing", "complete", "n", "min",
                                         "max", "empty", "n_unique"))
  expect_identical(defaults$complex, c("missing", "complete", "n"))
  expect_identical(defaults$Date, c("missing", "complete", "n", "min", "max",
                                  "median", "n_unique"))
  expect_identical(defaults$difftime, c("missing", "complete", "n", "min",
                                        "max", "median", "n_unique"))
  expect_identical(defaults$factor, c("missing", "complete", "n", "ordered",
                                      "n_unique", "top_counts"))
  expect_identical(defaults$integer, c("missing", "complete", "n", "mean", "sd",
                                       "p0", "p25", "p50", "p75", "p100",
                                       "hist"))
  expect_identical(defaults$list, c("missing", "complete", "n", "n_unique",
                                    "min_length", "max_length"))
  expect_identical(defaults$logical, c("missing", "complete", "n", "mean",
                                       "count"))
  expect_identical(defaults$numeric, c("missing", "complete", "n", "mean", "sd",
                                       "p0", "p25", "p50",  "p75", "p100",
                                       "hist"))
  expect_identical(defaults$POSIXct, c("missing", "complete", "n", "min", "max",
                                       "median", "n_unique"))
  expect_identical(defaults$ts, c("missing", "complete", "n", "start", "end",
                                  "frequency", "deltat",  "mean", "sd", "min",
                                  "max", "median", "line_graph"))
})

test_that("You can get the default skimmers for a particular class", {
  input <- get_default_skimmers("numeric")
  expect_named(input, "numeric")
  expect_identical(input$numeric, c("missing", "complete", "n", "mean", "sd",
                                    "p0", "p25", "p50",  "p75", "p100", "hist"))
})

test_that("You can get the default skimmers for multiple classes", {
  input <- get_default_skimmers(c("list", "AsIs"))
  expect_named(input, c("list", "AsIs"))
  expect_identical(input$AsIs, c("missing", "complete", "n", "n_unique",
                                 "min_length", "max_length"))
  expect_identical(input$list, c("missing", "complete", "n", "n_unique",
                                 "min_length", "max_length"))
})

test_that("A warning is given for classes that don't have defaults", {
  expect_warning(input <- get_default_skimmers(c("no_class")), "defaults")
})
