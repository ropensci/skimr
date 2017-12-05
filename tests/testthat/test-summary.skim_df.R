context("Make and print a summary of a skim object")

test_that("Summary creates the correct summary object", {
  # Test it creates the correct 4 parts
  input <- summary(skim(iris))
  expect_named(input, c("df_name", "n_rows", "n_cols", "type_frequencies"))
  
})

test_that("The summary print method prints the correct object", {
  # Test that the correct lines are output
  input <- summary(skim(iris))
  
  expect_output(print(input), "A skim object")
  
  expect_output(print(input), "Name: iris")
  expect_output(print(input), "Number of Rows: 150")
  expect_output(print(input), "Number of Columns: 5")
  
  expect_output(print(input), "Column type frequency")
  expect_output(print(input), "factor: 1")
  expect_output(print(input), "numeric: 4")
})
