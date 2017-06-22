context("Print a skim_df object")

data("CO2")
CO2$conc <- as.integer(CO2$conc)
CO2$Type <- as.character(CO2$Type)
skim_object <- skim(CO2)
sk_print_object<-skim_print(skim(CO2))
# We need to this modification bthat happens in skim_print() to successfully run print_handling(). 
skim_object$stat <- ifelse(skim_object$level == ".all" | skim_object$stat == "hist" | skim_object$stat == "count",  skim_object$stat,  paste(skim_object$stat, skim_object$level))
#---- Ordered Factor

correct <- tibble::tribble(
  ~var,   ~complete, ~missing, ~n,  ~n_unique, ~counts,
  "Plant", 84,        0,       84,  12,        "Qn1:7 Qn2:7 Qn3:7 Qc1:7 Qc3:7 Qc2:7 Mn3:7 Mn2:7 Mn1:7 Mc2:7 Mc3:7 Mc1:7 NA:0"
)

test_that("print_handling() returns expected response for ordered factor vectors", {
  input <- print_handling$sk_print_ordered(skim_object[skim_object$type == "ordered",])
  expect_identical(input, correct)
})
attr(correct, "subtibble_title") <- "Ordered Variables\n"

test_that("skim_print returns expected response (adds subtibble_title attribute) for ordered factor vectors", {
  input <- sk_print_object[["ordered"]]
  expect_identical(input, correct)
})

#--- Factor
correct <- tibble::tribble(
  ~var,         ~complete,~missing, ~n,  ~n_unique,   ~counts,
  "Treatment",  84,        0,       84,   2,           "nonchilled:42 chilled:42 NA:0"
  
)

test_that("print_handling() returns expected response for factor vectors", {
  input <- sk_print_factor(skim_object[skim_object$type == "factor",])
  expect_identical(input, correct)
})

attr(correct, "subtibble_title") <- "Factor Variables\n"

test_that("skim_print() returns expected response for factor vectors", {
  input <- sk_print_object[["factor"]]
  expect_identical(input, correct)
})


#---- Numeric
correct <- tibble::tribble(
  ~var,    ~missing, ~complete, ~n, ~mean,             ~sd,             ~min,  ~median,   ~`quantile 25%`, ~`quantile 75%`,~max, ~hist,
  "uptake", 0,        84,       84,  mean(CO2$uptake), sd(CO2$uptake), 7.7,   28.3,      17.9,           37.125,         45.5, "▅▇▆▅▂▅▇▆▇▅"
)

test_that("print_handling() returns expected response for numeric vectors", {
  input <- sk_print_numeric(skim_object[skim_object$type == "numeric",])
  expect_identical(input, correct)
})

attr(correct, "subtibble_title") <- "Numeric Variables\n"
test_that("skim_print() returns expected response for numeric vectors", {
  input <- sk_print_object[["numeric"]]
  expect_identical(input, correct)
})

#---- Character
correct <- tibble::tribble(
  ~var,   ~missing, ~complete, ~n,   ~min, ~max, ~empty, ~n_unique,
  "Type",  0,        84,         84,  6,    11,    0,       2
)
test_that("print_handling() returns expected response for character vectors", {
  input <- sk_print_character(skim_object[skim_object$type == "character",])
  expect_identical(input, correct)
})
attr(correct, "subtibble_title") <- "Character Variables\n"
test_that("skim_print() returns expected response for character vectors", {
  input <- sk_print_object[["character"]]
  expect_identical(input, correct)
})

#--- Integer
correct <- tibble::tribble(
~var,   ~missing, ~complete,  ~n,  ~mean,  ~sd,              ~min,  ~median,  ~`quantile 25%`,~`quantile 75%`,~max, ~hist,
"conc",  0,        84,        84,    435,  sd(CO2$conc), 95,     350,     175,            675,            1000, "▇▃▃▁▃▁▃▁▁▃"
)
correct_integer <- correct
test_that("print_handling() returns expected response for integer vectors", {
  input <- sk_print_integer(skim_object[skim_object$type == "integer",])
  expect_identical(input, correct)
})
attr(correct, "subtibble_title") <- "Integer Variables\n"
test_that("print_handling() returns expected response for integer vectors", {
  input <- sk_print_object[["integer"]]
  expect_identical(input, correct)
})

CO2$conc <- as.numeric(CO2$conc)
test_df <- CO2[c("conc", "Plant")]
skim_object3 <- skim(test_df)
# We need to this modification that happens in skim_print() to successfully run print_handling(). 
skim_object3$stat <- ifelse(skim_object3$level == ".all" | skim_object3$stat == "hist" | skim_object3$stat == "count",  skim_object3$stat,  paste(skim_object3$stat, skim_object3$level))
skim_print_object3 <- skim_print(skim_object3)

test_that("print_handling() returns the same tibble response for integer and numeric vectors", {
  input <- print_handling[["sk_print_numeric"]](skim_object3[skim_object3$type == "numeric",])
  expect_identical(input, correct_integer)
})

#---- Ts (example of a type with no custom method)
data("freeny")
skim_object2 <- skim(freeny)
sk_print_object2<-skim_print(skim_object2)
# We need to this modification that happens in skim_print() to successfully run print_handling(). 
skim_object2$stat <- ifelse(skim_object2$level == ".all" | skim_object2$stat == "hist" | skim_object2$stat == "count",  skim_object2$stat,  paste(skim_object2$stat, skim_object2$level))

correct <- tibble::tribble(
  ~var,  ~missing,  ~complete,  ~n,  ~start,  ~end,  ~frequency,  ~deltat,  ~mean,           ~sd,         ~min,     ~max,   ~median,
  "y",    0,         39,        39,   1962,    1971,  4,           0.25,     mean(freeny$y), sd(freeny$y), 8.79137, 9.79424, 9.31378
)
test_that("print_handling() returns expected response for ts vectors", {
  input <- sk_print_default(skim_object2[skim_object2$type == "ts",])
  expect_identical(input, correct)
})
attr(correct, "subtibble_title") <- "Ts Variables\n"
test_that("skim_print() returns expected response for ts vectors", {
  input <- sk_print_object2[["ts"]]
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~var,         ~complete,~missing, ~n,  ~n_unique,   ~counts,
  "Treatment",  84,        0,       84,   2,           "nonchilled:42 chilled:42 NA:0",
  "Type",       84,        0,       84,   2,          "Quebec:42 Mississippi:42 NA:0"
)
test_that("print handling returns correct response when there are multiple factor variables", {
          data(CO2)
          test_df <- CO2[c("Treatment", "Type")]
          input<-sk_print_factor(skim(test_df))
          expect_identical(input, correct)
} )