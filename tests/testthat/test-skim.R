context("Skim a data.frame")

# Target output -----------------------------------------------------------

correct <- tibble::tribble(
  ~var,     ~type,     ~stat,      ~level,       ~value, ~formatted_value,
  "weight", "numeric",  "missing",   ".all",      0,        "0",
  "weight", "numeric",  "complete",  ".all",       71,    "71",
  "weight", "numeric",  "n",         ".all",       71,    "71",
  "weight", "numeric",  "mean",      ".all",    mean(chickwts$weight), as.character(mean(chickwts$weight)),
  "weight", "numeric",  "sd",        ".all",     sd(chickwts$weight), as.character(sd(chickwts$weight)),
  "weight", "numeric",  "min",       ".all",       108,   "108",
  "weight", "numeric",  "median",    ".all",      258,    "258",
  "weight","numeric",  "quantile",  "25%",       quantile(chickwts$weight, probs=c(.25)),  as.character(quantile(chickwts$weight, probs=c(.25))),
  "weight", "numeric",  "quantile",  "75%",      quantile(chickwts$weight, probs=c(.75)),  as.character(quantile(chickwts$weight, probs=c(.75))),
  "weight", "numeric",  "max",       ".all",    423,      "423",
  "weight", "numeric",  "hist",      ".all",        0,     "▂▇▂▇▇▃▇▆▂▂",
  "feed",   "factor",   "missing",   ".all",        0,    "0",
  "feed",   "factor",   "complete",  ".all",       71,    "71",
  "feed",   "factor",   "n",         ".all",       71,    "71",
  "feed",   "factor",   "count",     "casein",     12,    "12",
  "feed",   "factor",   "count",     "horsebean",  10,    "10",
  "feed",   "factor",   "count",     "linseed",    12,    "12",
  "feed",   "factor",   "count",     "meatmeal",   11,    "11",
  "feed",   "factor",   "count",     "soybean",    14,    "14",
  "feed",   "factor",   "count",     "sunflower",  12,    "12",
  "feed",   "factor",   "count",      NA,           0,     "0",
  "feed",   "factor",   "n_unique",  ".all",        6,     "6"
  
)
  
class(correct) <- c("skim_df", class(correct))

reference_printed_output <- c("Numeric Variables",
                              "# A tibble: 1 × 13",
                              "     var    type missing complete     n     mean      sd   min `25% quantile` median",
                              "   <chr>   <chr>   <dbl>    <dbl> <dbl>    <dbl>   <dbl> <dbl>          <dbl>  <dbl>",
                              "1 weight numeric       0       71    71 261.3099 78.0737   108          204.5    258",
                              "# ... with 3 more variables: `75% quantile` <dbl>, max <dbl>, hist <chr>",
                              "",
                              "Factor Variables",
                              "# A tibble: 1 × 7",
                              "    var   type complete missing     n n_unique",
                              "  <chr>  <chr>    <dbl>   <dbl> <dbl>    <dbl>",
                              "1  feed factor       71       0    71        6",
                              "# ... with 1 more variables: stat <chr>")

# Begin tests -------------------------------------------------------------
skim_with_defaults()

test_that("Skimming a data frame works as expected", {
  input <- skim(chickwts)
  expect_equal(input, correct)
})

test_that("Using skim_tee returns the object", {
  printed_output <- capture.output({ skim_object <- skim_tee(chickwts) })
  expect_identical(chickwts, skim_object)
})

test_that("Using skim_tee prints out the object", {
  printed_output <- capture.output({ skim_object <- skim_tee(chickwts) })
  
  expect_identical(reference_printed_output[1], printed_output[1])
  
})
correct_grouped <- tibble::tribble(
  ~cyl,~gear,~var,~type,~stat,~level,~value,~formatted_value,
  6,4,"mpg","numeric","min",".all",17.8,"17.8",
  6,4,"mpg","numeric","max",".all",21,"21",
  6,4,"mpg","numeric","complete",".all",4,"4",
  6,4,"disp","numeric","min",".all",160,"160",
  6,4,"disp","numeric","max",".all",167.6,"167.6",
  6,4,"disp","numeric","complete",".all",4,"4",
  6,4,"hp","numeric","min",".all",110,"110",
  6,4,"hp","numeric","max",".all",123,"123",
  6,4,"hp","numeric","complete",".all",4,"4",
  6,4,"drat","numeric","min",".all",3.9,"3.9",
  6,4,"drat","numeric","max",".all",3.92,"3.92",
  6,4,"drat","numeric","complete",".all",4,"4",
  6,4,"wt","numeric","min",".all",2.62,"2.62",
  6,4,"wt","numeric","max",".all",3.44,"3.44",
  6,4,"wt","numeric","complete",".all",4,"4",
  6,4,"qsec","numeric","min",".all",16.46,"16.46",
  6,4,"qsec","numeric","max",".all",18.9,"18.9",
  6,4,"qsec","numeric","complete",".all",4,"4",
  6,4,"vs","numeric","min",".all",0,"0",
  6,4,"vs","numeric","max",".all",1,"1",
  6,4,"vs","numeric","complete",".all",4,"4",
  6,4,"am","numeric","min",".all",0,"0",
  6,4,"am","numeric","max",".all",1,"1",
  6,4,"am","numeric","complete",".all",4,"4",
  6,4,"carb","numeric","min",".all",4,"4",
  6,4,"carb","numeric","max",".all",4,"4",
  6,4,"carb","numeric","complete",".all",4,"4",
  4,4,"mpg","numeric","min",".all",21.4,"21.4",
  4,4,"mpg","numeric","max",".all",33.9,"33.9",
  4,4,"mpg","numeric","complete",".all",8,"8",
  4,4,"disp","numeric","min",".all",71.1,"71.1",
  4,4,"disp","numeric","max",".all",146.7,"146.7",
  4,4,"disp","numeric","complete",".all",8,"8",
  4,4,"hp","numeric","min",".all",52,"52",
  4,4,"hp","numeric","max",".all",109,"109",
  4,4,"hp","numeric","complete",".all",8,"8",
  4,4,"drat","numeric","min",".all",3.69,"3.69",
  4,4,"drat","numeric","max",".all",4.93,"4.93",
  4,4,"drat","numeric","complete",".all",8,"8",
  4,4,"wt","numeric","min",".all",1.615,"1.615",
  4,4,"wt","numeric","max",".all",3.19,"3.19",
  4,4,"wt","numeric","complete",".all",8,"8",
  4,4,"qsec","numeric","min",".all",18.52,"18.52",
  4,4,"qsec","numeric","max",".all",22.9,"22.9",
  4,4,"qsec","numeric","complete",".all",8,"8",
  4,4,"vs","numeric","min",".all",1,"1",
  4,4,"vs","numeric","max",".all",1,"1",
  4,4,"vs","numeric","complete",".all",8,"8",
  4,4,"am","numeric","min",".all",0,"0",
  4,4,"am","numeric","max",".all",1,"1",
  4,4,"am","numeric","complete",".all",8,"8",
  4,4,"carb","numeric","min",".all",1,"1",
  4,4,"carb","numeric","max",".all",2,"2",
  4,4,"carb","numeric","complete",".all",8,"8",
  6,3,"mpg","numeric","min",".all",18.1,"18.1",
  6,3,"mpg","numeric","max",".all",21.4,"21.4",
  6,3,"mpg","numeric","complete",".all",2,"2",
  6,3,"disp","numeric","min",".all",225,"225",
  6,3,"disp","numeric","max",".all",258,"258",
  6,3,"disp","numeric","complete",".all",2,"2",
  6,3,"hp","numeric","min",".all",105,"105",
  6,3,"hp","numeric","max",".all",110,"110",
  6,3,"hp","numeric","complete",".all",2,"2",
  6,3,"drat","numeric","min",".all",2.76,"2.76",
  6,3,"drat","numeric","max",".all",3.08,"3.08",
  6,3,"drat","numeric","complete",".all",2,"2",
  6,3,"wt","numeric","min",".all",3.215,"3.215",
  6,3,"wt","numeric","max",".all",3.46,"3.46",
  6,3,"wt","numeric","complete",".all",2,"2",
  6,3,"qsec","numeric","min",".all",19.44,"19.44",
  6,3,"qsec","numeric","max",".all",20.22,"20.22",
  6,3,"qsec","numeric","complete",".all",2,"2",
  6,3,"vs","numeric","min",".all",1,"1",
  6,3,"vs","numeric","max",".all",1,"1",
  6,3,"vs","numeric","complete",".all",2,"2",
  6,3,"am","numeric","min",".all",0,"0",
  6,3,"am","numeric","max",".all",0,"0",
  6,3,"am","numeric","complete",".all",2,"2",
  6,3,"carb","numeric","min",".all",1,"1",
  6,3,"carb","numeric","max",".all",1,"1",
  6,3,"carb","numeric","complete",".all",2,"2",
  4,3,"mpg","numeric","min",".all",21.5,"21.5",
  4,3,"mpg","numeric","max",".all",21.5,"21.5",
  4,3,"mpg","numeric","complete",".all",1,"1",
  4,3,"disp","numeric","min",".all",120.1,"120.1",
  4,3,"disp","numeric","max",".all",120.1,"120.1",
  4,3,"disp","numeric","complete",".all",1,"1",
  4,3,"hp","numeric","min",".all",97,"97",
  4,3,"hp","numeric","max",".all",97,"97",
  4,3,"hp","numeric","complete",".all",1,"1",
  4,3,"drat","numeric","min",".all",3.7,"3.7",
  4,3,"drat","numeric","max",".all",3.7,"3.7",
  4,3,"drat","numeric","complete",".all",1,"1",
  4,3,"wt","numeric","min",".all",2.465,"2.465",
  4,3,"wt","numeric","max",".all",2.465,"2.465",
  4,3,"wt","numeric","complete",".all",1,"1",
  4,3,"qsec","numeric","min",".all",20.01,"20.01",
  4,3,"qsec","numeric","max",".all",20.01,"20.01",
  4,3,"qsec","numeric","complete",".all",1,"1",
  4,3,"vs","numeric","min",".all",1,"1",
  4,3,"vs","numeric","max",".all",1,"1",
  4,3,"vs","numeric","complete",".all",1,"1",
  4,3,"am","numeric","min",".all",0,"0",
  4,3,"am","numeric","max",".all",0,"0",
  4,3,"am","numeric","complete",".all",1,"1",
  4,3,"carb","numeric","min",".all",1,"1",
  4,3,"carb","numeric","max",".all",1,"1",
  4,3,"carb","numeric","complete",".all",1,"1"
  
   )

mtcars_test <- dplyr::filter(mtcars, cyl < 8, gear   < 5)
newfuns <- list(min = min, max = max, complete = n_complete)
skim_with(numeric = newfuns, append = FALSE)
input <- mtcars_test %>%  dplyr::group_by(cyl, gear) %>% skim()

test_that("Skimming a grouped data frame works as expected (value)", {
  expect_equal(input$value, correct_grouped$value)
})

test_that("Skimming a grouped data frame works as expected (cyl)", {
  expect_identical(input$cyl, correct_grouped$cyl) 
})

test_that("Skimming a grouped data frame works as expected (gear)", {
  expect_identical(input$gear, correct_grouped$gear) 
})
test_that("Skimming a grouped data frame works as expected (var)", {
  expect_identical(input$var, correct_grouped$var) 
})
test_that("Skimming a grouped data frame works as expected (stat)", {
  expect_identical(input$stat, correct_grouped$stat) 
})
test_that("Skimming a grouped data frame works as expected (level)", {
  expect_identical(input$level, correct_grouped$level) 
})
test_that("Grouped data get the correct class",{
  expect_identical(class(input), c("skim_df", "grouped_df", "tbl_df", "tbl", "data.frame"))
})
