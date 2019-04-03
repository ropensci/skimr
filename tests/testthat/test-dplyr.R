context("Using dplyr verbs on skim objects works as expected")

skimmed_iris <- skim(iris)

test_that("dplyr::filter works as expected", {
  input <- dplyr::filter(skimmed_iris, skim_type == "numeric")
  expect_print_matches_file(input, "dplyr/filter-skim.txt")

  no_rows <- dplyr::filter(skimmed_iris, skim_type == "no_type")
  expect_print_matches_file(no_rows, "dplyr/filter-no-skim.txt")
})

test_that("dplyr::select works as expected", {
  with_type <- dplyr::select(skimmed_iris, skim_type, skim_variable)
  expect_print_matches_file(with_type, "dplyr/select-skim.txt")

  without_type <- dplyr::select(skimmed_iris, mean)
  expect_print_matches_file(without_type, "dplyr/select-no-skim.txt")
})

test_that("dplyr::mutate works as expected", {
  input <- dplyr::mutate(skimmed_iris, mean2 = mean^2)
  expect_print_matches_file(input, "dplyr/mutate-skim.txt")

  no_variable <- dplyr::mutate(skimmed_iris, skim_variable = NULL)
  expect_print_matches_file(no_variable, "dplyr/mutate-no-skim.txt")
})

test_that("dplyr::slice works as expected", {
  input <- dplyr::slice(skimmed_iris, 1:3)
  expect_print_matches_file(input, "dplyr/slice.txt")
})

test_that("dplyr::arrange works as expected", {
  input <- dplyr::arrange(skimmed_iris, desc(mean))
  expect_print_matches_file(input, "dplyr/arrange.txt")
})
