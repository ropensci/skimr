context("Using dplyr verbs on skim objects works as expected")
# In v2 the base results are a skim_df object with nrow equal to the number of
# variables and length of the total number of unique statistics across types
# plus 2.

skimmed_iris <- skim(iris)
nvar_iris <- length(iris)
skimmers_used_iris <- attr(skimmed_iris, "skimmers_used")
ntypes <- length(skimmers_used_iris)
skimmers_iris <- union(skimmers_used_iris[[1]], skimmers_used_iris[[2]])
length_skimmed_iris <- length(skimmers_iris) + 2

test_that("dplyr::filter works as expected (returns wide data frame)", {
  result <- dplyr::filter(skimmed_iris, type == "numeric")
  expect_n_columns(result, length_skimmed_iris)
  expect_n_rows(result, nvar_iris - 1)
})

test_that("dplyr::select works as expected (returns wide data frame)", {
  result <- dplyr::select(skimmed_iris, type, variable)
  expect_n_columns(result, 2)
  expect_n_rows(result, nvar_iris)
})

test_that("dplyr::mutate works as expected (returns wide data frame)", {
  result <- dplyr::mutate(skimmed_iris, mean2 = mean^2)
  expect_n_columns(result, length_skimmed_iris + 1)
  expect_n_rows(result, nvar_iris)
})

test_that("dplyr::slice works as expected (returns wide data frame)", {
  result <- dplyr::slice(skimmed_iris, 1:3)
  expect_n_columns(result, length_skimmed_iris)
  expect_n_rows(result, 3)
})

test_that("dplyr::arrange works as expected (returns wide data frame)", {
  result <- dplyr::arrange(skimmed_iris, desc(mean))
  expect_n_rows(result, nvar_iris)
  expect_n_columns(result, length_skimmed_iris)
  expect_equal(as.numeric(result[1, "mean"]), max(result$mean, na.rm = TRUE))
  expect_equal(is.na(as.numeric(result[5, "mean"])), TRUE)
})
