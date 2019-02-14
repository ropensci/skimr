context("Print a skim_df object")

test_that("Skim prints a header for the entire output and each type", {
  input <- skim(iris)
  expect_output(print(input), "Data summary")
  expect_output(print(input), "                       Values")
  expect_output(print(input), "Name                   `iris`")
  expect_output(print(input), "Number of rows            150")
  expect_output(print(input), "Number of columns           5")
  expect_output(print(input), "Column type frequency        ")  
  expect_output(print(input), "factor                      1")  
  expect_output(print(input), "numeric                     4")  
  expect_output(print(input), "── Variable type: factor ────────────────")
  expect_output(print(input), "── Variable type: numeric ────────────────")
})

test_that("Skim prints a special header for grouped data frames", {
  input <- skim(dplyr::group_by(iris, Species))
  expect_output(print(input), "Name                   `dplyr::group_by\\(iris, Species\\)`")
  expect_output(print(input), "Number of rows                                      150")
  expect_output(print(input), "Number of columns                                     5")
  expect_output(print(input), "Group variables                                 Species")
})

test_that("Skim lists print as expected", {
  skimmed <- skim(iris)
  input <- partition(skimmed)
  expect_output(print(input), "\\$factor")
  expect_output(print(input), "── Variable type: factor ────────────────")
  expect_output(print(input), "\\$numeric")
  expect_output(print(input), "── Variable type: numeric ───────────────")
})

test_that("knit_print produced expected results", {
  skimmed <- skim(iris)
  input <- knit_print(skimmed)
  expect_is(input, "knit_asis")
  expect_length(input, 1)
  expect_match(
    input,
    "|skim_variable | missing| complete|   n|ordered | n_unique|top_counts"
  )
})

test_that("knit_print works with skim summaries", {
  skimmed <- skim(iris)
  summarized <- summary(skimmed)
  multi_line <- capture.output(knit_print(summarized))
  input <- paste(multi_line, collapse = "")
  #expect_match(input, "Data summary    ", fixed = TRUE)
  #expect_match(input, "df_name")
  expect_output(print(input), "|Name                  |`iris` |")
  expect_output(print(input), "|Number of rows        |150    |")
  expect_output(print(input), "Number of columns     |5      |")
  expect_match(input, "|Column type frequency |       |", fixed = TRUE)
  expect_match(input, "|factor                |1      |")
  expect_match(input, "|numeric               |4      |")
})

test_that("Summaries can be suppressed within knitr", {
  skimmed <- skim(iris)
  options <- list(skimr_include_summary = FALSE)
  input <- knit_print(skimmed, options = options)
  expect_false(grepl("Data summary", input))
})

test_that("Skim lists have a separate knit_print method", {
  skimmed <- skim(iris)
  skim_list <- partition(skimmed)
  input <- knit_print(skim_list)
  expect_match(input, "\n\n\n**Variable type: factor**\n\n", fixed = TRUE)
  expect_match(input, "\n\n\n**Variable type: numeric**\n\n", fixed = TRUE)
})

test_that("You can yank a type from a skim_df and call knit_print", {
  skimmed <- skim(iris)
  skim_one <- yank(skimmed, "factor")
  input <- knit_print(skim_one)
  expect_match(input, "\n\n**Variable type: factor**\n\n", fixed = TRUE)
  expect_false(grepl("\n\n**Variable type: numeric**\n\n", input, fixed = TRUE))
})

test_that("make_utf8 produces the correct result ", {
  input <- make_utf8(c("<U+2585><U+2587>"))
  correct <- "▅"
  expect_identical(input, correct)
})

test_that("Skim falls back to tibble::print.tbl() appropriately", {
  input <- skim(iris)
  mean_only <- dplyr::select(input, mean)
  expect_output(print(mean_only), "# A tibble: 5 x 1")
})

test_that("Print focused objects appropriately", {
  skimmed <- skim(iris)
  input <- focus(skimmed, missing)
  expect_output(print(input), "Name                   `iris`")
  expect_output(print(input), "Number of rows            150")
  expect_output(print(input), "Number of columns           5")
  expect_output(print(input), "Column type frequency        ")  
  expect_output(print(input), "factor                      1")  
  expect_output(print(input), "numeric                     4")    
})
