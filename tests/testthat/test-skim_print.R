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

test_that("Skimr kable prints as expected", {
  skimmed <- skim(iris)
  input <- capture.output(skimr::kable(skimmed))
  expect_length(input, 15)
  
  expected <- character()
  expected[1] <- ""
  expected[2] <- "Variable type: factor"
  expected[3] <- ""
  expected[4] <- "|variable |missing |complete |n   |n_unique |top_counts                       |ordered |"
  expected[5] <- "|:--------|:-------|:--------|:---|:--------|:--------------------------------|:-------|"
  expected[6] <- "|Species  |0       |150      |150 |3        |set: 50, ver: 50, vir: 50, NA: 0 |FALSE   |"
  expected[7] <- ""
  expected[8] <- "Variable type: numeric"
  expected[9] <- ""
  expected[10] <- "|variable     |missing |complete |n   |mean |sd   |min |p25 |median |p75 |max |hist     |"
  expected[11] <- "|:------------|:-------|:--------|:---|:----|:----|:---|:---|:------|:---|:---|:--------|"
  expected[12] <- "|Petal.Length |0       |150      |150 |3.76 |1.77 |1   |1.6 |4.35   |5.1 |6.9 |▇▁▁▂▅▅▃▁ |"
  expected[13] <- "|Petal.Width  |0       |150      |150 |1.2  |0.76 |0.1 |0.3 |1.3    |1.8 |2.5 |▇▁▁▅▃▃▂▂ |"
  expected[14] <- "|Sepal.Length |0       |150      |150 |5.84 |0.83 |4.3 |5.1 |5.8    |6.4 |7.9 |▂▇▅▇▆▅▂▂ |"
  expected[15] <- "|Sepal.Width  |0       |150      |150 |3.06 |0.44 |2   |2.8 |3      |3.3 |4.4 |▁▂▅▇▃▂▁▁ |"

  expect_equal(input, expected)  
})


test_that("skimr::pander prints as expected", {
  # This assumes the default option for line length (80).
  
  input <- utils::capture.output(skim(chickwts) %>% pander() )
  expect_equal(length(input), 35)
  correct <- character()
  correct[1] <-  ""
  correct[2] <-  "Variable type: factor"
  correct[3] <-  "----------------------------------------------------------------------------"
  correct[4] <-  " variable   missing   complete   n    n_unique           top_counts         "
  correct[5] <-  "---------- --------- ---------- ---- ---------- ----------------------------"
  correct[6] <-  "   feed        0         71      71      6       soy: 14, cas: 12, lin: 12, "
  correct[7] <-  "                                                          sun: 12           "
  correct[8] <-  "----------------------------------------------------------------------------"
  correct[9] <-  ""
  correct[10] <-  "Table: Table continues below"
  correct[11] <-  ""
  correct[12] <-  " "
  correct[13] <-  "---------"
  correct[14] <-  " ordered "
  correct[15] <-  "---------"
  correct[16] <-  "  FALSE  "
  correct[17] <-  "---------"
  correct[18] <-  ""
  correct[19] <-  ""
  correct[20] <-  "Variable type: numeric"
  correct[21] <-  "----------------------------------------------------------------------------"
  correct[22] <-  " variable   missing   complete   n     mean     sd     min    p25    median "
  correct[23] <-  "---------- --------- ---------- ---- -------- ------- ----- ------- --------"
  correct[24] <-  "  weight       0         71      71   261.31   78.07   108   204.5    258   "
  correct[25] <-  "----------------------------------------------------------------------------"
  correct[26] <-  ""
  correct[27] <-  "Table: Table continues below"
  correct[28] <-  ""
  correct[29] <-  " "
  correct[30] <-  "------------------------"
  correct[31] <-  "  p75    max     hist   "
  correct[32] <-  "------- ----- ----------"
  correct[33] <-  " 323.5   423   ▃▅▅▇▃▇▂▂ "
  correct[34] <-  "------------------------"
  correct[35] <-  ""
  expect_equal(input, correct)
})
