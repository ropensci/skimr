context("Print a skim_df object")

test_that("Skim prints a header for the entire output and each type", {
  input <- skim(iris)
  expect_output(print(input), "Skim summary statistics")
  expect_output(print(input), "n obs: 150")
  expect_output(print(input), "n variables: 5")
  expect_output(print(input), "── Variable type: factor ────────────────")
  expect_output(print(input), "── Variable type: numeric ────────────────")
})

test_that("Skim prints a special header for grouped data frames", {
  input <- skim(dplyr::group_by(iris, Species))
  expect_output(print(input), " group variables: Species")
  expect_output(print(input), "n obs: 150")
  expect_output(print(input), "n variables: 5")
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

  expect_match(input, "**Skim summary statistics**", fixed = TRUE)
  expect_match(input, "<table style='width: auto;'")
  expect_match(input, "class='table table-condensed'>")
  expect_match(input, " <thead>")
  expect_match(input, "  <tr>")
  expect_match(input, "   <th style=\"text-align:right;\"> n_obs </th>")
  expect_match(input, "   <th style=\"text-align:right;\"> n_cols </th>")
  expect_match(input, "  </tr>")
  expect_match(input, " </thead>")
  expect_match(input, "<tbody>")
  expect_match(input, "  <tr>")
  expect_match(input, "   <td style=\"text-align:right;\"> 150 </td>")
  expect_match(input, "   <td style=\"text-align:right;\"> 5 </td>")
  expect_match(input, "  </tr>")
  expect_match(input, "</tbody>")
  expect_match(input, "</table>")
  expect_match(input, "**Variable type: factor**", fixed = TRUE)
  expect_match(input,
    "|variable | missing| complete|   n|ordered | n_unique|top_counts")
})

test_that("Summaries can be suppressed within knitr", {
  skimmed <- skim(iris)
  options <- list(skimr_include_summary = FALSE)
  input <- knit_print(skimmed, options = options)
  expect_false(grepl("Skim summary statistics", input))
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
