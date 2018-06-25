context("Make and print a summary of a skim object")

test_that("Summary creates the correct summary object", {
  # Test it creates the correct 4 parts
  skim_input <- skim(iris)
  summary_input <- summary(skim_input)
  expect_named(summary_input, c("df_name", "n_rows", "n_cols",
                                "type_frequencies"))
})

test_that("The summary print method prints the correct object", {
  # Test that the correct lines are output
  skim_input <- skim(iris)
  summary_input <- summary(skim_input)
  
  expect_output(print(summary_input), "A skim object")
  
  expect_output(print(summary_input), "Name: `iris`")
  expect_output(print(summary_input), "Number of Rows: 150")
  expect_output(print(summary_input), "Number of Columns: 5")
  
  expect_output(print(summary_input), "Column type frequency")
  expect_output(print(summary_input), "factor: 1")
  expect_output(print(summary_input), "numeric: 4")
})

test_that("The summary print method prints the correct object when piped", {
  # Test that the correct lines are output, no name should be output.
  summary_input <- skim(iris) %>%
    summary()

  expect_output(print(summary_input), "A skim object")
  expect_output(print(summary_input), "Number of Rows: 150")
  expect_output(print(summary_input), "Number of Columns: 5")
  expect_output(print(summary_input), "Column type frequency")
  expect_output(print(summary_input), "factor: 1")
  expect_output(print(summary_input), "numeric: 4")
})

test_that("null object gets expected message", {
  input <- NULL
  expect_error(summary.skim_df(input), "dataframe is null.")
})
