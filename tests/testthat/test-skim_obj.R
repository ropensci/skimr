context("Checks for a skimr object")

test_that("skim produces skim_df objects", {
  skimmed <- skim(iris)
  expect_true(is_skim_df(skimmed))
  expect_false(is_skim_list(skimmed))
  expect_error(assert_is_skim_df(skimmed), NA)
  expect_error(assert_is_skim_list(skimmed))
})

test_that("Partition produces skim_list objects", {
  skimmed <- skim(iris)
  split <- partition(skimmed)
  expect_false(is_skim_df(split))
  expect_true(is_skim_list(split))
  expect_error(assert_is_skim_df(split))
  expect_error(assert_is_skim_list(split), NA)
})

test_that("Bind produces skim_df objects", {
  skimmed <- skim(iris)
  split <- partition(skimmed)
  combined <- bind(split)
  expect_true(is_skim_df(combined))
  expect_false(is_skim_list(combined))
  expect_error(assert_is_skim_df(combined), NA)
  expect_error(assert_is_skim_list(combined))
})

test_that("Creating a skim_df requires type and variable columns", {
  expect_true(could_be_skim_df(
    data.frame(
      type = "t",
      variable = "v"
    )
  ))
  expect_false(could_be_skim_df(
    list(
      type = "t",
      variable = "v"
    )
  ))
  expect_false(could_be_skim_df(
    data.frame(
      variable = "v"
    )
  ))
  expect_false(could_be_skim_df(
    data.frame(
      type = "t"
    )
  ))
})
