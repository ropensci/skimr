context("Reshaping a skim_df")

test_that("You can parition a skim_df", {
  skimmed <- skim(iris)
  input <- partition(skimmed)
  expect_is(input, "skim_list")
  expect_length(input, 2)
  expect_named(input, c("factor", "numeric"))
  attrs <- attributes(input)
  expect_equal(attrs$data_rows, 150)
  expect_equal(attrs$data_cols, 5)
  expect_identical(attrs$df_name, "`iris`")
  expect_identical(attrs$skimmers_used,
               list(numeric = c("missing", "complete", "n", "mean", "sd", "p0",
                                "p25", "p50", "p75", "p100", "hist"),
                    factor = c("missing", "complete", "n", "ordered",
                               "n_unique", "top_counts")))
  
  # Subtables
  expect_is(input$factor, c("one_skim_df", "tbl_df", "tbl", "data.frame"))
  expect_n_rows(input$factor, 1)
  expect_n_columns(input$factor, 7)
  expect_named(input$factor, c("variable", "missing", "complete", "n",
                               "ordered", "n_unique", "top_counts"))
  
  expect_is(input$numeric, c("one_skim_df", "tbl_df", "tbl", "data.frame"))
  expect_n_rows(input$numeric, 4)
  expect_n_columns(input$numeric, 12)
  expect_named(input$numeric, c("variable", "missing", "complete", "n", "mean",
                                "sd", "p0", "p25", "p50", "p75", "p100",
                                "hist"))
})

test_that("Partitioning works in a round trip", {
  skimmed <- skim(iris)
  partitioned <- partition(skimmed)
  input <- bind(partitioned)
  expect_equal(input, skimmed)
})

test_that("You can yank a subtable from a skim_df", {
  skimmed <- skim(iris)
  input <- yank(skimmed, "numeric")
  expect_is(input, c("one_skim_df", "tbl_df", "tbl", "data.frame"))
  expect_n_rows(input, 4)
  expect_n_columns(input, 12)
  expect_named(input, c("variable", "missing", "complete", "n", "mean",
                        "sd", "p0", "p25", "p50", "p75", "p100",
                        "hist"))
})
