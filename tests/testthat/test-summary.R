context("Make and print a summary of a skim object")

test_that("Summary creates the correct summary object", {
  # Test it creates the correct 4 parts
  skim_input <- skim(iris)
  summary_input <- summary(skim_input)
  expect_equal(rownames(summary_input), c(
    "Name", "Number of rows ", "Number of columns ",
    "_______________________ ",
    "Column type frequency: ", "  factor", "  numeric",
    "________________________  ",
    "Group variables"
  ))
})

test_that("The summary print method prints the correct object", {
  skim_summary_input <- summary(skim(iris))
  expect_known_output(
    skim_summary_input, "summary/summary_iris.txt",
    update = FALSE,
    print = TRUE
  )
})

test_that("The summary print method prints the correct object when piped", {
  # Test that the correct lines are output, no name should be output.
  summary_input <- iris %>%
    skim() %>%
    summary()
  expect_known_output(summary_input, "summary/summary_iris_piped.txt",
    update = FALSE, print = TRUE
  )
})

test_that("null object gets expected message", {
  input <- NULL
  expect_error(summary.skim_df(input), "dataframe is null.")
})
