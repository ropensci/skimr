context("Print a skim_df object")

test_that("Skim prints a header for the entire output and each type", {
  input <- skim(iris)
  expect_output(print(input), "Skim summary statistics")
  expect_output(print(input), "n obs: 150")
  expect_output(print(input), "n variables: 5 \n")
  expect_output(print(input), "Variable type: factor")
  expect_output(print(input), "Variable type: numeric")
})

test_that("Skim prints a special header for grouped data frames", {
  input <- skim(dplyr::group_by(iris, Species))
  expect_output(print(input), " group variables: Species")
  expect_output(print(input), "n obs: 150")
  expect_output(print(input), "n variables: 5 \n")
})

test_that("Skim prints a special header for vectors", {
  input <- skim(lynx)
  expect_output(print(input), "Skim summary statistics")
  expect_output(print(input), "Variable type: ts")
})

test_that("Skim collapses counts and other multivalue stats into one cell", {
  input <- skim(iris)
  expect_output(print(skim, "set: 50, ver: 50, vir: 50, NA: 0"))
})

test_that("Skim aligns numeric vectors at the decimal point by default", {
  input <- skim(mtcars)
  expect_output(print(input), "  am       0       32 32   0.41   0.5   0")
  expect_output(print(input), "carb       0       32 32   2.81   1.62  1")
  expect_output(print(input), "drat       0       32 32   3.6    0.53  2.76")
  expect_output(print(input), "gear       0       32 32   3.69   0.74  3")
})

test_that("spark.print returns the correct result",{
  input <- inline_hist(chickwts$weight)
  expect_output(print(input), "▃▅▅▇▃▇▂▂")
  
})
