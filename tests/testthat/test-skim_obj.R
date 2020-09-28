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
  expect_error(assert_is_skim_df(split), "not a data.frame")
  expect_error(assert_is_skim_list(split), NA)

  expect_true(is_one_skim_df(split[[1]]))
  expect_error(assert_is_one_skim_df(split[[1]]), NA)
})

test_that("Error messages are correct", {
  expect_error(assert_is_skim_df(iris), "missing column `skim_type`")
  expect_error(assert_is_skim_df(iris), "missing column `skim_variable`")
  expect_error(assert_is_skim_df(iris), "missing attributes:")
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

test_that("Creating a skim_df requires skim_type and skim_variable columns", {
  expect_true(could_be_skim_df(
    data.frame(
      skim_type = "t",
      skim_variable = "v"
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
