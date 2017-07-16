context("Print a skim_df object")

data("CO2")
CO2$conc <- as.integer(CO2$conc)
CO2$Type <- as.character(CO2$Type)
skim_object <- skim(CO2)
sk_print_object<-skim_print(skim_object)
# We need to this modification that happens in skim_print() to successfully run print_handling().
skim_object$stat <- ifelse(skim_object$level == ".all" |  skim_object$stat == "count",
                              skim_object$stat,  paste(skim_object$stat, skim_object$level))
#---- Ordered Factor

correct <- tibble::tribble(
  ~var,   ~complete, ~missing, ~n,  ~n_unique, ~counts,
  "Plant", "84",        "0",       "84",  "12", "Qn1:7 Qn2:7 Qn3:7 Qc1:7 Qc3:7 Qc2:7 Mn3:7 Mn2:7 Mn1:7 Mc2:7 Mc3:7 Mc1:7 NA:0"
)

test_that("print_handling() returns expected response for ordered factor vectors", {
  input <- sk_print_factor(skim_object[skim_object$type == "ordered",])
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
  "Treatment",  "84",        "0", "84",   "2",           "nonchilled:42 chilled:42 NA:0"

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
withr::with_locale(c(LC_COLLATE = "C"), code = '
correct <- tibble::tribble(
  ~var,    ~missing, ~complete, ~n,             ~mean,                  ~sd,                         ~min,  ~median,   ~`quantile 25%`, ~`quantile 75%`,~max, ~hist,
  "uptake", "0",        "84",   "84",  "27.2130952380952", "10.8144122908108", "7.7", "28.3",       "17.9",       "37.125",       "45.5", "▅▇▆▅▂▅▇▆▇▅"
)

test_that("print_handling() returns expected response for numeric vectors", {
  input <- sk_print_default(skim_object[skim_object$type == "numeric",])
  expect_equal(input[,1:4], correct[,1:4])
})

attr(correct, "subtibble_title") <- "Numeric Variables\n"
test_that("skim_print() returns expected response for numeric vectors", {
  input <- sk_print_object[["numeric"]]
  expect_identical(input, correct)
})
')

#---- Character
correct <- tibble::tribble(
  ~var,   ~missing, ~complete, ~n,   ~min, ~max, ~empty, ~n_unique,
  "Type",  "0",        "84",   "84",  "6",    "11",    "0",       "2"
)
test_that("print_handling() returns expected response for character vectors", {
  input <- sk_print_default(skim_object[skim_object$type == "character",])
  expect_identical(input, correct)
})
attr(correct, "subtibble_title") <- "Character Variables\n"
test_that("skim_print() returns expected response for character vectors", {
  input <- sk_print_object[["character"]]
  expect_identical(input, correct)
})

#--- Integer
correct <- tibble::tribble(
~var,   ~missing, ~complete,  ~n,  ~mean,  ~sd,                        ~min,  ~median,  ~`quantile 25%`,~`quantile 75%`,~max, ~hist,
"conc",  "0",        "84",    "84", "435",  as.character(sd(CO2$conc)), "95",     "350",     "175",       "675",        "1000", "▇▃▃▁▃▁▃▁▁▃"
)
correct_integer <- correct
test_that("print_handling() returns expected response for integer vectors", {
  input <- sk_print_default(skim_object[skim_object$type == "integer",])
  expect_identical(input, correct)
})
attr(correct, "subtibble_title") <- "Integer Variables\n"
test_that("print_handling() returns expected response for integer vectors", {
  input <- sk_print_object[["integer"]]
  expect_identical(input, correct)
})


#---- Ts (example of a type with no custom method)
data("freeny")
skim_object2 <- skim(freeny)
sk_print_object2<-skim_print(skim_object2)
# We need to this modification that happens in skim_print() to successfully run print_handling().
skim_object2$stat <- ifelse(skim_object2$level == ".all" |  skim_object2$stat == "count",  
                            skim_object2$stat,  paste(skim_object2$stat, skim_object2$level))

correct <- tibble::tribble(
  ~var,~missing,~complete,~n,~start,~end,~frequency,~deltat,~mean,~sd,~min,~max,~median,~line_graph,
  "y","0","39","39","1962","1971","4","0.25",as.character(mean(freeny$y)),as.character(sd(freeny$y)),"8.79137","9.79424","9.31378","⣀⣀⣀⣀⣀⠤⠤⠤⠤⠔⠒⠒⠒⠒⠉⠉⠉⠉⠉⢁"
)
test_that("print_handling() returns expected response for 1:13 ts vectors", {
  input <- sk_print_default(skim_object2[skim_object2$type == "ts",])
  expect_equal(input, correct)
})

test_that("skim_print() returns expected title for ts vectors", {
  input <- sk_print_object2[["ts"]]
  expect_equal(attr(input, "subtibble_title"), "Ts Variables\n")
})

data(CO2)
test_df <- CO2[c("Treatment", "Type")]
test_df$Treatment <- as.factor(test_df$Treatment)
test_df$Type <- as.factor(test_df$Type)
withr::with_locale(c(LC_COLLATE = "C"), code = '
correct <- tibble::tribble(
  ~var,         ~complete,~missing, ~n,  ~n_unique,   ~counts,
  "Treatment",  "84",        "0",       "84",   "2",           "nonchilled:42 chilled:42 NA:0",
  "Type",       "84",        "0",       "84",   "2",          "Mississippi:42 Quebec:42 NA:0"
)
test_that("print handling returns correct response when there are multiple factor variables", {

          input<-sk_print_factor(skim_object4)
          expect_identical(dim(input), dim(correct))
          expect_identical(input$counts, correct$counts)
} )
'
)

# --- Grouped data

correct <- tibble::tribble(
  ~var,~cyl,~gear,~missing, ~complete,  ~n,  ~mean,  ~sd,~min,~median,~`quantile 25%`,~`quantile 75%`,~max,~hist,
  "am",  "4",  "3",  "0",    "1",        "1",  "0",    NA,"0",  "0",      "0",           "0","0","",
  "am",  "4",  "4",  "0",    "8",        "8",  "0.75","0.462910049886276","0","1","0.75","1","1","▂▁▁▁▁▁▁▁▁▇",
  "am",  "4",  "5",  "0",    "2",        "2",  "1",    "0","1",  "1",      "1",           "1","1","▁▁▁▁▇▁▁▁▁▁",
  "am",  "6",  "3"  ,"0",    "2",        "2",  "0",    "0","0",  "0",      "0",           "0","0","",
  "am",  "6",  "4",  "0",    "4",        "4",  "0.5",  "0.577350269189626","0","0.5","0","1","1","▇▁▁▁▁▁▁▁▁▇",
  "am",  "6",  "5",  "0",    "1",        "1",  "1",    NA,"1",   "1",      "1",           "1","1","▁▁▁▁▇▁▁▁▁▁",
  "am",  "8",  "3",  "0",    "12",      "12",  "0",    "0","0",  "0",      "0",           "0","0","",
  "am",  "8",  "5",  "0",    "2",        "2",  "1",    "0","1",  "1",      "1",           "1","1","▁▁▁▁▇▁▁▁▁▁",
  "carb","4",  "3",  "0",    "1",        "1",  "1",    NA,"1",  "1",       "1",           "1","1","▁▁▁▁▇▁▁▁▁▁",
  "carb","4",  "4",  "0",    "8",        "8",  "1.5","0.534522483824849",  "1","1.5","1","2","2","▇▁▁▁▁▁▁▁▁▇",
  "carb","4",  "5",  "0",    "2",        "2",  "2",    "0","2",  "2",      "2",           "2","2","▁▁▁▁▇▁▁▁▁▁",
  "carb","6",  "3",  "0",    "2",        "2",  "1",    "0","1",  "1",      "1",           "1","1","▁▁▁▁▇▁▁▁▁▁"
)

data(mtcars)
skim_with_defaults()
skim_object5 <- mtcars %>% dplyr::group_by(cyl, gear) %>% skim()
skim_print_object5<-print(skim_object5)

test_that("print handling returns correct response with grouped data.", {
   input <- skim_print_object5$numeric
   input <- input[1:12,]
   
  expect_equal(input, correct, tolerance =.01)
})

test_that("skim_print() returns expected title for grouped data", {

  input <- skim_print_object5$numeric
   expect_equal(attr(input, "subtibble_title"), "Numeric Variables grouped by cyl, gear \n")
})

