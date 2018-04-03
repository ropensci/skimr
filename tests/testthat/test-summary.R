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
  
  expect_output(print(summary_input), "Name: iris")
  expect_output(print(summary_input), "Number of Rows: 150")
  expect_output(print(summary_input), "Number of Columns: 5")
  
  expect_output(print(summary_input), "Column type frequency")
  expect_output(print(summary_input), "factor: 1")
  expect_output(print(summary_input), "numeric: 4")
})

test_that("The summary print method prints the correct object when piped", {
  # Test that the correct lines are output, no name should be output.
  summary_input <- skim(iris) %>% summary()
  
  expect_output(print(summary_input), "A skim object")
  
  expect_output(print(summary_input), "Number of Rows: 150")
  expect_output(print(summary_input), "Number of Columns: 5")
  
  expect_output(print(summary_input), "Column type frequency")
  expect_output(print(summary_input), "factor: 1")
  expect_output(print(summary_input), "numeric: 4")
})

test_that("The summary pander method returns the correct object", {
  s <- skim(iris)  %>% summary()
  pander_input <- capture.output(pander(s))
  expect_equal(pander_input[3], "  * Name: iris")
  expect_equal(pander_input[4], "  * Number of Rows: 150")
  expect_equal(pander_input[5], "  * Number of Columns: 5")
  expect_equal(pander_input[6], "  *")
  expect_equal(pander_input[7], "")
  expect_equal(pander_input[8], "    -------------")
  expect_equal(pander_input[9], "      type     n")
  expect_equal(pander_input[10], "    --------- ---")
  expect_equal(pander_input[11], "     factor    1")
  expect_equal(pander_input[12], "")
  expect_equal(pander_input[13], "     numeric   4")
  expect_equal(pander_input[14], "    -------------")
  expect_equal(pander_input[15], "")
})

test_that("The summary kable method returns the correct object", {
  s <- skim(iris)  %>% summary()
  kable_input <- capture.output(kable(s))
  expect_equal(kable_input[1], "$Summary")
  expect_equal(kable_input[2], "")
  expect_equal(kable_input[3], "")
  expect_equal(kable_input[4], 
               "|  df_name   |       n_rows        |        n_cols        |")
  expect_equal(kable_input[5], 
               "|------------|---------------------|----------------------|")
  expect_equal(kable_input[6],
               "| Name: iris | Number of Rows: 150 | Number of Columns: 5 |")
  expect_equal(kable_input[7], "")
  expect_equal(kable_input[8], "$`Type counts`")
  expect_equal(kable_input[9], "")
  expect_equal(kable_input[10], "")
  expect_equal(kable_input[11], "|  type   | n |")
  expect_equal(kable_input[12], "|---------|---|" )
  expect_equal(kable_input[13], "| factor  | 1 |")
  expect_equal(kable_input[14], "| numeric | 4 |")
  expect_equal(kable_input[15], "")
})

test_that("null object gets expected message", {
  input <- NULL
  expect_error(summary.skim_df(input), "dataframe is null.")
  
})
