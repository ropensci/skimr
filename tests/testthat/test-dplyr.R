skimmed_iris <- skim(iris)

test_that("dplyr::filter works as expected", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  expect_snapshot({
    dplyr::filter(skimmed_iris, skim_type == "numeric")
    # no rows
    dplyr::filter(skimmed_iris, skim_type == "no_type")
  })
})

test_that("dplyr::select works as expected", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  with_type <- dplyr::select(skimmed_iris, skim_type, skim_variable)
  expect_snapshot(with_type)
  without_type <- dplyr::select(skimmed_iris, numeric.mean)
  expect_snapshot(without_type)
})

test_that("dplyr::mutate works as expected", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  input <- dplyr::mutate(skimmed_iris, mean2 = numeric.mean^2)
  expect_snapshot(input)

  no_variable <- dplyr::mutate(skimmed_iris, skim_variable = NULL)
  identical(
    capture.output(print(no_variable)),
    capture.output(print(tibble::as_tibble(no_variable)))
  )
})

test_that("dplyr::slice works as expected", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  input <- dplyr::slice(skimmed_iris, 1:3)
  expect_snapshot(input)
})

test_that("dplyr::arrange works as expected", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  expect_snapshot(dplyr::arrange(skimmed_iris, desc(numeric.mean)))
})
