context("Using dplyr verbs on skim objects works as expected")

skimmed_iris <- skim(iris)

test_that("dplyr::filter works as expected", {
  input <- dplyr::filter(skimmed_iris, skim_type == "numeric")
  expect_output(print(input), "Skim summary statistics")
  
  no_rows <- dplyr::filter(skimmed_iris, skim_type == "no_type")
  expect_output(print(no_rows), "# A tibble: 0 x 16")
})

test_that("dplyr::select works as expected", {
  with_type <- dplyr::select(skimmed_iris, skim_type, skim_variable)
  expect_output(print(with_type), "Skim summary statistics")

  without_type <- dplyr::select(skimmed_iris, mean)
  expect_output(print(without_type), "# A tibble")
})

test_that("dplyr::mutate works as expected", {
  input <- dplyr::mutate(skimmed_iris, mean2 = mean^2)
  expect_output(print(input), "Skim summary statistics")
  
  no_variable <- dplyr::mutate(skimmed_iris, skim_variable = NULL)
  expect_output(print(no_variable), "# A tibble: 5 x 15")
})

test_that("dplyr::slice works as expected", {
  input <- dplyr::slice(skimmed_iris, 1:3)
  expect_output(print(input), "Skim summary statistics")
})

test_that("dplyr::arrange works as expected", {
  input <- dplyr::arrange(skimmed_iris, desc(mean))
  expect_output(print(input), "Skim summary statistics")
  expect_output(print(input), "1 Sepal.Length")
  expect_output(print(input), "2 Petal.Length")
  expect_output(print(input), "3 Sepal.Width")
  expect_output(print(input), "4 Petal.Width")
})
