
test_that("skim of a simple data.table produces no warnings", {
  skip_if_not_installed("data.table")
  withr::local_options(list(width = 91))
  DT_letters <- data.table::data.table(abc = letters)
  expect_no_warning(skim(DT_letters))
})

test_that("skim of a simple data.table produces no warnings even with dtplyr", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dtplyr")
  withr::local_options(list(width = 91))
  DT_letters <- data.table::data.table(abc = letters)
  expect_no_warning(skim(DT_letters))
})

test_that("skim of a simple data.table produces output as expected", {
  skip_if_not_installed("data.table")
  DT_letters <- data.table::data.table(abc = letters)

  skimmed_DT_letters <- skim(DT_letters)
  withr::local_options(list(cli.unicode = FALSE, width = 91))
  expect_snapshot(skimmed_DT_letters)
})


test_that("skim of data.table produces output as expected", {
  skip_if_not_installed("data.table")
  set.seed(1L)

  DT_factors <- data.table::data.table(
    abc = letters,
    grps = factor(sample(c("AA", "BB"), 26, TRUE)),
    values = rnorm(26)
  )

  withr::local_options(list(cli.unicode = FALSE, width = 91))
  expect_snapshot(skim(DT_factors))

  data.table::setkeyv(DT_factors, c("abc", "grps"))
  expect_snapshot(skim(DT_factors))

  DF_factors <- as.data.frame(DT_factors)
  expect_snapshot(skim(DF_factors))

  tibble_factors <- tibble::as_tibble(DT_factors)
  expect_snapshot(skim(tibble_factors))
})
