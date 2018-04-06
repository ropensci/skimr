context("Formatting options")

test_formats_that("Default formats are correct", {
  formats <- show_formats()
  expect_identical(formats$.levels, list(max_char = 3, max_levels = 4))
  expect_true(formats$.align_decimal)
  expect_identical(formats$numeric, list(digits = 2, nsmall = 2,
                                          drop0trailing = TRUE))
  expect_identical(formats$integer, list(drop0trailing = TRUE))
  expect_identical(formats$character, list(width = 8))
  expect_identical(formats$date, list(format = "%Y-%m-%d"))
  expect_identical(formats$posixct, list(format = "%Y-%m-%d"))
  expect_identical(formats$logical, list())
  expect_identical(formats$asis, list())
})

test_formats_that("Formatting options can be changed", {
  skim_format(numeric = list(digits = 1, nsmall = 1), .align_decimal = FALSE,
              append = FALSE)
  formats <- show_formats()
  expect_identical(formats$numeric, list(digits = 1, nsmall = 1))
  expect_false(formats$.align_decimal)
  expect_identical(formats$integer, list(drop0trailing = TRUE))
})

test_formats_that("Multiple formats can be changed", {
  skim_format(numeric = list(digits = 1), integer = list(digits = 1),
              append = FALSE) 
  formats <- show_formats()
  expect_identical(formats$numeric, list(digits = 1))
  expect_identical(formats$integer, list(digits = 1))
})

test_formats_that("Multiple formats can be changed, list", {
  new_formats <- list(numeric = list(digits = 1), integer = list(digits = 1))
  skim_format(.list = new_formats, append = FALSE) 
  formats <- show_formats()
  expect_identical(formats$numeric, list(digits = 1))
  expect_identical(formats$integer, list(digits = 1))
})

test_formats_that("Formatting options change printed output", {
  skim_format(numeric = list(digits = 2, nsmall = 2), .align_decimal = FALSE,
              append = FALSE)
  input <- skim(mtcars)
  expect_output(print(input), "am       0       32 32   0.41   0.50  0.00")
  expect_output(print(input), "carb       0       32 32   2.81   1.62  1.00")
})

test_formats_that("Warning message is correctly returned when there is 
          no format for a type", {
  expect_warning(skimr:::get_formats("unknown_type"), 
       "Skimr does not know how to format type: unknown_type. Leaving as is.")
})
