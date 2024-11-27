test_that("Using skim_tee prints returns the object", {
  skip_if_not(l10n_info()$`UTF-8`)
  expect_snapshot({
    skim_object <- skim_tee(chickwts)
  })
  expect_identical(chickwts, skim_object)
})

test_that("skim_tee prints only selected columns, but returns full object", {
  skip_if_not(l10n_info()$`UTF-8`)
  expect_snapshot({
    obj <- skim_tee(iris, Species)
    obj
  })
  expect_identical(obj, iris)
})

test_that("skim_tee supports dplyr helpers", {
  skip_if_not(l10n_info()$`UTF-8`)
  expect_snapshot({
    obj <- skim_tee(iris, starts_with("Sepal"))
  })
  expect_identical(obj, iris)
})

test_that("Skim_tee works with groups", {
  skip_if_not(l10n_info()$`UTF-8`)
  iris_grouped <- dplyr::group_by(iris, Species)
  my_skim <- skim_with(numeric = sfl(hist = NULL))
  expect_snapshot({
    obj <- skim_tee(iris_grouped, Sepal.Length, skim_fun = my_skim)
  })
  expect_identical(obj, iris_grouped)
})
