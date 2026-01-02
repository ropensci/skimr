test_that("Skim prints a header for the entire output and each type", {
  withr::local_options(list(cli.unicode = FALSE))
  skip_if_not(l10n_info()$`UTF-8`)
  expect_snapshot({
    input <- skim(iris)
    input
    input$numeric.hist <- NULL
    input
  })
})

test_that("Skim prints a special header for grouped data frames", {
  skip_if_not(l10n_info()$`UTF-8`)
  skip_on_os("mac")
  withr::local_options(list(cli.unicode = FALSE))
  expect_snapshot( skim(dplyr::group_by(iris, Species)))
})

test_that("Skim lists print as expected", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  expect_snapshot(partition(skimmed))
})

test_that("knit_print produces expected results", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  input <- knit_print(skimmed)
  expect_s3_class(input, "knit_asis")
  expect_length(input, 1)
  expect_snapshot(cat(input))
})

test_that("knit_print works with skim summaries", {
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  summarized <- summary(skimmed)
  expect_snapshot(cat(knitr::knit_print(summarized)))
})

test_that("knit_print appropriately falls back to tibble printing", {
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  reduced <- dplyr::select(skimmed, skim_variable, numeric.mean)
  expect_snapshot({
    input <- knitr::knit_print(reduced)
  })
  expect_s3_class(input, "data.frame")
})

test_that("Summaries can be suppressed within knitr", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  options <- list(skimr_include_summary = FALSE)
  expect_snapshot(cat(knitr::knit_print(skimmed, options = options)))
})

test_that("Skim lists have a separate knit_print method", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  skim_list <- partition(skimmed)
  expect_snapshot(cat(knit_print(skim_list)))
})

test_that("You can yank a type from a skim_df and call knit_print", {
  withr::local_options(list(cli.unicode = FALSE))
  skimmed <- skim(iris)
  skim_one <- yank(skimmed, "factor")
  expect_snapshot(cat(knit_print(skim_one)))
})

test_that("Skim falls back to tibble::print.tbl() appropriately", {
  withr::local_options(list(cli.unicode = FALSE))
  
  expect_snapshot({
    input <- skim(iris)
    dplyr::select(input, numeric.mean)
  })
})

test_that("Print focused objects appropriately", {
  withr::local_options(list(cli.unicode = FALSE))
  skip_if_not(l10n_info()$`UTF-8`)
  skimmed <- skim(iris)
  expect_snapshot(focus(skimmed, n_missing))
})

test_that("Support for smaller consoles can be set with the width option", {
  withr::local_options(list(cli.unicode = FALSE))
  skip_if_not(l10n_info()$`UTF-8`)
  expect_snapshot(skim(iris))
})

test_that("Table header width can be controlled by an option", {
  withr::local_options(list(cli.unicode = FALSE))
  skip_if_not(l10n_info()$`UTF-8`)
  skimmed <- skim(iris)
  expect_snapshot(skimmed)
})

test_that("skimr creates appropriate output for Jupyter", {
  withr::local_options(list(cli.unicode = FALSE))
  skip_if_not(l10n_info()$`UTF-8`)
  skimmed <- skim(iris)
  expect_snapshot(skimmed)
})
