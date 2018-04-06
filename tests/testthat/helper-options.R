test_functions_that <- function(desc, code) {
  skim_with_defaults()
  test_that(desc, code)
  skim_with_defaults()
}

test_formats_that <- function(desc, code) {
  skim_format_defaults()
  test_that(desc, code)
  skim_format_defaults()
}
