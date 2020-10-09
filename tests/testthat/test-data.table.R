context("Skim of a data table")

width_bak <- getOption("width")
options(width = 91)

test_that("skim of a simple data.table produces no warnings", {
  skip_if_not(require(data.table))
  DT_letters <- data.table::data.table(abc = letters)
  expect_warning(skim(DT_letters), NA)
})

test_that("skim of a simple data.table produces no warnings even with dtplyr", {
  skip_if_not(require(data.table))
  skip_if_not(require(dtplyr))
  DT_letters <- data.table::data.table(abc = letters)
  expect_warning(skim(DT_letters), NA)
})

test_that("skim of a simple data.table produces output as expected", {
  skip_if_not(require(data.table))
  DT_letters <- data.table::data.table(abc = letters)

  skimmed_DT_letters <- skim(DT_letters)
  withr::local_options(list(cli.unicode = FALSE))
  expect_print_matches_file(skimmed_DT_letters, "data.table/summary_DT_letters.txt")
})


test_that("skim of data.table produces output as expected", {
  skip_if_not(require(data.table))
  set.seed(1L)

  DT_factors <- data.table::data.table(abc = letters
    , grps = factor(sample(c("AA", "BB"), 26, TRUE))
    , values = rnorm(26))

  withr::local_options(list(cli.unicode = FALSE))

  expect_print_matches_file(skim(DT_factors), "data.table/summary_DT_factors_no_key.txt")

  data.table::setkeyv(DT_factors, c("abc", "grps"))
  expect_print_matches_file(skim(DT_factors), "data.table/summary_DT_factors.txt")

  DF_factors <- as.data.frame(DT_factors)
  expect_print_matches_file(skim(DF_factors), "data.table/summary_DF_factors.txt")

  tibble_factors <- tibble::as_tibble(DT_factors)
  expect_print_matches_file(skim(tibble_factors), "data.table/summary_tibble_factors.txt")
})

options(width = width_bak)
