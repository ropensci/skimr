test_that("You can bind skim_df rows", {
  skimmed <- skim(iris)
  combined <- dplyr::bind_rows(skimmed, skimmed)
})
