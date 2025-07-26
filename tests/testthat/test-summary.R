test_that("Summary creates the correct summary object", {
  withr::local_options(list(cli.unicode = FALSE))
  # Test it creates the correct 4 parts
  skim_input <- skim(iris)
  summary_input <- summary(skim_input)
  expect_named(
    summary_input,
    c(
      "data_name", "counts", "types", "possible_groups",
      "dt_key", "data_rows", "data_cols"
    )
  )
  expect_identical(summary_input$data_name, "iris")
  expect_identical(summary_input$types, c("factor", "numeric"))
  expect_identical(summary_input$data_rows, 150L)
  expect_identical(summary_input$data_cols, 5L)
})

test_that("The summary print method prints the correct object", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  skim_summary_input <- summary(skim(iris))
  expect_snapshot(skim_summary_input)
})

test_that("The summary print method prints the correct object when piped", {
  skip_if_not(l10n_info()$`UTF-8`)
  withr::local_options(list(cli.unicode = FALSE))
  # Test that the correct lines are output, no name should be output.
  summary_input <- iris |>
    skim() |>
    summary()
  expect_snapshot(summary_input)
})

test_that("null object gets expected message", {
  input <- NULL
  expect_error(summary.skim_df(input), "dataframe is null.")
})
