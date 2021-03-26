test_that("You can bind skim_df rows", {
  skimmed1 <- skim(iris)
  skimmed2 <- skim(mtcars)
  combined <- vctrs::vec_rbind(skimmed1, skimmed2)
  expect_s3_class(combined, "skim_df")

  attrs <- attributes(combined)
  expect_equal(attrs$data_rows, 182)
  expect_equal(attrs$data_cols, 16)
  expect_equal(attrs$df_name, "`iris`+`mtcars`")
  expect_equal(
    attrs$skimmers_used,
    list(
      numeric = c("mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist"),
      factor = c("ordered", "n_unique", "top_counts")
    )
  )
})

test_that("When binding columns, fall back to tbl_df", {
  skimmed <- skim(iris)
  combined <- vctrs::vec_cbind(skimmed, skimmed)
  expect_s3_class(combined, "tbl")
  expect_false("skim_df" %in% class(combined))
})
