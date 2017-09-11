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
  "Plant",    "84",     "0",  "84",       "12", "Qn1:7 Qn2:7 Qn3:7 Qc1:7 Qc3:7 Qc2:7 Mn3:7 Mn2..."
)

test_that("print_handling() returns expected response for ordered factor vectors", {
  input <- sk_print_object[[ "factor"]]
  expect_identical(input[1,], correct)
})

#--- Factor
correct <- tibble::tribble(
  ~var,         ~complete,~missing, ~n,  ~n_unique,   ~counts,
  "Plant", "84",        "0",       "84",  "12", "Qn1:7 Qn2:7 Qn3:7 Qc1:7 Qc3:7 Qc2:7 Mn3:7 Mn2...",
  "Treatment",  "84",        "0", "84",   "2",           "nonchilled:42 chilled:42 NA:0"

)

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

attr(correct, "subtibble_title") <- "Character Variables\n"
test_that("skim_print() returns expected response for character vectors", {
  input <- sk_print_object[["character"]]
  expect_identical(input, correct)
})

#--- Integer
correct <- tibble::tribble(
~var,   ~missing, ~complete,  ~n,  ~mean,  ~sd,    ~min,  ~median,  ~`quantile 25%`,~`quantile 75%`,~max, ~hist,
"conc",  "0",        "84",    "84", "435.0", "295.9", "95",     "350.0",     "175.0",       "675.0",      "1000", "▇▃▃▁▃▁▃▁▁▃"
)
correct_integer <- correct

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
  "y","0","39","39","1962","1971","4","0.25","  9.3","  0.3","8.79137","9.79424","  9.3","⣀⣀⣀⣀⣀⠤⠤⠤⠤⠔⠒⠒⠒⠒⠉⠉⠉⠉⠉⢁"
)

test_that("skim_print() returns expected title for ts vectors", {
  input <- sk_print_object2[["ts"]]
  expect_equal(attr(input, "subtibble_title"), "Ts Variables\n")
})

data(CO2)
test_df <- CO2[c("Treatment", "Type")]
test_df$Treatment <- as.factor(test_df$Treatment)
test_df$Type <- as.factor(test_df$Type)
skim_object4 <-skim(test_df)
skim_object4$stat <- ifelse(skim_object4$level == ".all" |  skim_object4$stat == "count",
                           skim_object4$stat,  paste(skim_object4$stat, skim_object4$level))

# We need to do this because different OSes will sort the factor labels differently.
withr::with_locale(c(LC_COLLATE = "C"), code = '
correct <- tibble::tribble(
  ~var,         ~complete,~missing, ~n,  ~n_unique,   ~counts,
  "Treatment",  "84",        "0",       "84",   "2",           "nonchilled:42 chilled:42 NA:0",
  "Type",       "84",        "0",       "84",   "2",          "Mississippi:42 Quebec:42 NA:0"
)
test_that("print handling returns correct response when there are multiple factor variables", {

    input<-skim_print(skim_object4)[["factor"]]
    expect_identical(dim(input), dim(correct))
    expect_identical(input$counts, correct$counts)
} )
'
)

# --- Grouped data

correct <- tibble::tribble(
  ~var,~cyl,~gear,~missing, ~complete,  ~n,  ~mean,       ~sd,~min,~median,~`quantile 25%`,~`quantile 75%`,~max,~hist,
  "am",  "4",  "3",  "0",    "1",        "1",  "  0.0", "   NA", "  0    ",  "  0.0",      "  0.0",  "  0.0",     "  0    ","",
  "am",  "4",  "4",  "0",    "8",        "8",  "  0.8", "  0.5", "  0    ",  "  1.0",      "  0.8",  "  1.0",    "  1    ","▂▁▁▁▁▁▁▁▁▇",
  "am",  "4",  "5",  "0",    "2",        "2",  "  1.0", "  0.0", "  1    ",  "  1.0",      "  1.0",  "  1.0",    "  1    ","▁▁▁▁▇▁▁▁▁▁",
  "am",  "6",  "3"  ,"0",    "2",        "2",  "  0.0", "  0.0", "  0    ",  "  0.0",      "  0.0",  "  0.0",    "  0    ","",
  "am",  "6",  "4",  "0",    "4",        "4",  "  0.5", "  0.6", "  0    ",  "  0.5",      "  0.0",  "  1.0",    "  1    ","▇▁▁▁▁▁▁▁▁▇",
  "am",  "6",  "5",  "0",    "1",        "1",  "  1.0", "   NA", "  1    ",  "  1.0",       "  1.0",  "  1.0",    "  1    ","▁▁▁▁▇▁▁▁▁▁",
  "am",  "8",  "3",  "0",    "12",      "12",  "  0.0", "  0.0", "  0    ",  "  0.0",       "  0.0",  "  0.0",    "  0    ","",
  "am",  "8",  "5",  "0",    "2",        "2",  "  1.0", "  0.0", "  1    ",  "  1.0",       "  1.0",  "  1.0",    "  1    ","▁▁▁▁▇▁▁▁▁▁",
  "carb","4",  "3",  "0",    "1",        "1",  "  1.0", "   NA", "  1    ",  "  1.0",       "  1.0",  "  1.0",    "  1    ","▁▁▁▁▇▁▁▁▁▁",
  "carb","4",  "4",  "0",    "8",        "8",  "  1.5", "  0.5", "  1    ",  "  1.5",       "  1.0",  "  2.0",    "  2    ","▇▁▁▁▁▁▁▁▁▇",
  "carb","4",  "5",  "0",    "2",        "2",  "  2.0", "  0.0", "  2    ",  "  2.0",       "  2.0",  "  2.0",    "  2    ","▁▁▁▁▇▁▁▁▁▁",
  "carb","6",  "3",  "0",    "2",        "2",  "  1.0", "  0.0", "  1    ",  "  1.0",       "  1.0",  "  1.0",    "  1    ","▁▁▁▁▇▁▁▁▁▁"
)

data(mtcars)
skim_with_defaults()
skim_object5 <- mtcars %>% dplyr::group_by(cyl, gear) %>% skim()
skim_print_object5<-skim_grouped_print(skim_object5)

test_that("print returns the same thing as skim_grouped_print",{
  input<-print.skim_grouped_df(skim_object5)
  correct <- skim_grouped_print(skim_object5)
  expect_equal(input, correct)

})

# 
test_that("print handling returns correct response with grouped data.", {
   input <- skim_print_object5$numeric
   input <- input[1:12,]
   
  expect_equal(input, correct, tolerance =.01)
})

test_that("skim_print() returns expected title for grouped data", {

  input <- skim_print_object5$numeric
   expect_equal(attr(input, "subtibble_title"), "Numeric Variables grouped by cyl, gear \n")
})

