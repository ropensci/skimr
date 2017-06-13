context("Print a skim_df object")

data("CO2")
CO2$conc <- as.integer(CO2$conc)
CO2$Type <- as.character(CO2$Type)
sk_print_object<-skim_print(skim(CO2))

#---- Ordered Factor

correct <- tibble::tribble(
  ~var,   ~complete, ~missing, ~n,  ~n_unique, ~counts,
  "Plant", 84,        0,       84,  12,        "Qn1:7 Qn2:7 Qn3:7 Qc1:7 Qc3:7 Qc2:7 Mn3:7 Mn2:7 Mn1:7 Mc2:7 Mc3:7 Mc1:7 NA:0"
  
)
attr(correct, "subtibble_title") <- "Ordered Variables\n"

test_that("print_handling() returns expected response for ordered factor vectors", {
  input <- sk_print_object[["ordered"]]
  expect_identical(input, correct)
})

#--- Factor
correct <- tibble::tribble(
  ~var,         ~complete,~missing, ~n,  ~n_unique,   ~counts,
  "Treatment",  84,        0,       84,   2,           "nonchilled:42 chilled:42 NA:0"
  
)
attr(correct, "subtibble_title") <- "Factor Variables\n"

test_that("print_handling() returns expected response for factor vectors", {
  input <- sk_print_object[["factor"]]
  expect_identical(input, correct)
})


#---- Numeric
correct <- tibble::tribble(
  ~var,    ~missing, ~complete, ~n, ~mean,             ~sd,             ~min,  ~median,   ~`quantile 25%`, ~`quantile 75%`,~max, ~hist,
  "uptake", 0,        84,       84,  mean(CO2$uptake), sd(CO2$uptake), 7.7,   28.3,      17.9,           37.125,         45.5, "▅▇▆▅▂▅▇▆▇▅"
)
attr(correct, "subtibble_title") <- "Numeric Variables\n"
test_that("print_handling() returns expected response for numeric vectors", {
  input <- sk_print_object[["numeric"]]
  expect_identical(input, correct)
})

#---- Character
correct <- tibble::tribble(
  ~var,   ~missing, ~complete, ~n,   ~min, ~max, ~empty, ~n_unique,
  "Type",  0,        84,         84,  6,    11,    0,       2
)
attr(correct, "subtibble_title") <- "Character Variables\n"
test_that("print_handling() returns expected response for character vectors", {
  input <- sk_print_object[["character"]]
  expect_identical(input, correct)
})

#--- Integer
correct <- tibble::tribble(
~var,   ~missing, ~complete,  ~n,  ~mean,  ~sd,              ~min,  ~median,  ~`quantile 25%`,~`quantile 75%`,~max, ~hist,
"conc",  0,        84,        84,    435,  sd(CO2$conc), 95,     350,     175,            675,            1000, "▇▃▃▁▃▁▃▁▁▃"
)
attr(correct, "subtibble_title") <- "Integer Variables\n"
test_that("print_handling() returns expected response for integer vectors", {
  input <- sk_print_object[["integer"]]
  expect_identical(input, correct)
})

#---- Ts (example of a type with no custom method)
data("freeny")
sk_print_object2<-skim_print(skim(freeny))
correct <- tibble::tribble(
  ~var,  ~missing,  ~complete,  ~n,  ~start,  ~end,  ~frequency,  ~deltat,  ~mean,           ~sd,         ~min,     ~max,   ~median,
  "y",    0,         39,        39,   1962,    1971,  4,           0.25,     mean(freeny$y), sd(freeny$y), 8.79137, 9.79424, 9.31378
)
attr(correct, "subtibble_title") <- "Ts Variables\n"
test_that("print_handling() returns expected response for ts vectors", {
  input <- sk_print_object2[["ts"]]
  expect_identical(input, correct)
})