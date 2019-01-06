context("Using dplyr verbs on skim objects works as expected")

skimmed_iris <- skim(iris)
# dplyr verbs in V1 of skimr should return long data frames _without_ the skim_df class.
test_that("dplyr::filter works as expected (returns long data frame)", {
  result <- dplyr::filter(skimmed_iris, stat == "mean")
  expect_equal(length(result), 6)
  expect_equal(nrow(result), 4)
})

test_that("dplyr::select works as expected (returns long data frame)", {
  result <- dplyr::select(skimmed_iris, stat, value)
  expect_equal(length(result), 2)
  expect_equal(nrow(result), 53)
})

test_that("dplyr::mutate works as expected (returns long data frame)", {
  result <- dplyr::mutate(skimmed_iris, value2 = value^2)
  expect_equal(length(result), 7)
  expect_equal(nrow(result), 53)
})

test_that("dplyr::slice works as expected (returns long data frame)", {
  result <- dplyr::slice(skimmed_iris, 1:5)
  expect_equal(length(result), 6)
  expect_equal(nrow(result), 5)
})

test_that("dplyr::arrange works as expected (returns long data frame)", {
  result <- dplyr::arrange(skimmed_iris, desc(value))
  expect_equal(length(result), 6)
  expect_equal(nrow(result), 53)
  expect_equal(as.numeric(result[1, "value"]), 150)
  expect_equal(is.na(as.numeric(result[53, "value"])), TRUE)
})
