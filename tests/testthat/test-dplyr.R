context("Using dplyr verbs on skim objects works as expected")
# dplyr version 0.8.0 produces a different set of classes than 
# prior versions.  This set of tests should assure that R 
# instances with either older or newer dplyr versions work in
# the same way.

skimmed_iris <- skim(iris)
nvar_iris <- length(iris)
skimmers_used_iris <- attr(skimmed_iris, "skimmers_used")
ntypes <- length(skimmers_used_iris)
skimmers_iris <- union(skimmers_used_iris[[1]], skimmers_used_iris[[2]])
length_skimmed_iris <- length(skimmers_iris) + 2
classes_skimmed_iris <- class(skimmed_iris)

test_that("dplyr::filter works as expected (returns wide data frame)", {
  result <- dplyr::filter(skimmed_iris, type == "numeric")
  expect_equal(length(result), length_skimmed_iris)
  expect_equal(nrow(result), nvar_iris - 1)
  expect_equal(class(result), classes_skimmed_iris)
})

test_that("dplyr::filter works as expected", {
  input <- dplyr::filter(skimmed_iris, type == "numeric")
  expect_output(print(input), "Skim summary statistics")
})

test_that("dplyr::select works as expected", {
  with_type <- dplyr::select(skimmed_iris, type, variable)
  expect_equal(class(with_type), classes_skimmed_iris)
  expect_equal(length(with_type), 2)
  expect_equal(nrow(with_type), nvar_iris)
  
  without_type <- dplyr::select(skimmed_iris, mean)
  # select should return a skim_df.
  expect_equal(class(without_type), classes_skimmed_iris)
  #expect_equal(class(without_type),  c("tbl_df", "tbl", "data.frame"))
})

test_that("dplyr::mutate works as expected", {
  input <- dplyr::mutate(skimmed_iris, mean2 = mean ^ 2)
  expect_equal(length(input), length_skimmed_iris + 1)
  expect_equal(nrow(input), nvar_iris)
  expect_equal(colnames(input)[length(input)], "mean2")
  expect_equal(class(input), classes_skimmed_iris)
})

test_that("dplyr::transmute works as expected", {
  # Should return variable, type and the created variable(s).
  with_meta_vars <- dplyr::transmute(skimmed_iris, complete2 = complete * 2, 
                                     variable = variable, type = type)
  expect_equal(length(with_meta_vars), 3)
  expect_equal(nrow(with_meta_vars), nvar_iris)
  expect_equal(colnames(with_meta_vars)[length(with_meta_vars)], "complete2")
  expect_equal(class(with_meta_vars), classes_skimmed_iris)
  
  without_meta_vars <- dplyr::transmute(skimmed_iris, complete2 = complete * 2)
  expect_equal(length(without_meta_vars), 3)
  expect_equal(nrow(without_meta_vars), nvar_iris)
  expect_equal(colnames(without_meta_vars)[length(without_meta_vars)], "complete2")
  expect_equal(class(without_meta_vars), classes_skimmed_iris)
})


test_that("dplyr::slice works as expected", {
  input <- dplyr::slice(skimmed_iris, 1:3)
  expect(length(input), length_skimmed_iris)
  expect(nrow(input), 3)
  expect_equal(class(input), classes_skimmed_iris)
})

test_that("dplyr::arrange works as expected", {
  input <- dplyr::arrange(skimmed_iris, desc(mean))
  expect(nrow(input), nvar_iris)
  expect(length(input), length_skimmed_iris)
  expect_equal(as.numeric(input[1, "mean"]), max(input$mean, na.rm = TRUE))
  expect_equal(is.na(as.numeric(input[5, "mean"])), TRUE)
  expect_equal(class(input), classes_skimmed_iris)
})
