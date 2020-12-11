test_that("Using skim_tee prints returns the object", {
  skip_if_not(l10n_info()$`UTF-8`)
  input <- testthat::capture_output(skim_object <- skim_tee(chickwts))
  expect_matches_file(input, "skim_tee/skim_tee.txt")
  expect_identical(chickwts, skim_object)
})

test_that("skim_tee prints only selected columns, but returns full object", {
  skip_if_not(l10n_info()$`UTF-8`)
  input <- testthat::capture_output(obj <- skim_tee(iris, Species))
  expect_matches_file(input, "skim_tee/species.txt")
  expect_identical(obj, iris)
})

test_that("skim_tee supports dplyr helpers", {
  skip_if_not(l10n_info()$`UTF-8`)
  input <- testthat::capture_output(obj <- skim_tee(iris, starts_with("Sepal")))
  expect_matches_file(input, "skim_tee/sepal.txt")
  expect_identical(obj, iris)
})

test_that("Skim_tee works with groups", {
  skip_if_not(l10n_info()$`UTF-8`)
  iris_grouped <- dplyr::group_by(iris, Species)
  my_skim <- skim_with(numeric = sfl(hist = NULL))
  input <- testthat::capture_output(
    obj <- skim_tee(iris_grouped, Sepal.Length, skim_fun = my_skim)
  )
  expect_matches_file(input, "skim_tee/grouped.txt")
  expect_identical(obj, iris_grouped)
})
