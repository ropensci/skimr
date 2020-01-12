context("Using skim_tee")

test_that("Using skim_tee prints returns the object", {
  skip_if_not(l10n_info()$`UTF-8`)
  expect_known_output(
    skim_object <- skim_tee(chickwts), "skim_tee/skim_tee.txt"
  )
  expect_identical(chickwts, skim_object)
})

test_that("skim_tee prints only selected columns, but returns full object", {
  skip_if_not(l10n_info()$`UTF-8`)
  expect_known_output(
    obj <- skim_tee(iris, Species), "skim_tee/species.txt"
  )
  expect_identical(obj, iris)
})

test_that("skim_tee supports dplyr helpers", {
  skip_if_not(l10n_info()$`UTF-8`)
  expect_known_output(
    obj <- skim_tee(iris, starts_with("Sepal")), "skim_tee/sepal.txt"
  )
  expect_identical(obj, iris)
})

test_that("Skim_tee works with groups", {
  skip_if_not(l10n_info()$`UTF-8`)
  iris_grouped <- dplyr::group_by(iris, Species)
  my_skim <- skim_with(numeric = sfl(hist = NULL))
  expect_known_output(
    obj <- skim_tee(iris_grouped, Sepal.Length, skim_fun = my_skim),
    "skim_tee/grouped.txt"
  )
  expect_identical(obj, iris_grouped)
})
