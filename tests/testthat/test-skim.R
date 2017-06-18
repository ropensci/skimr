context("Skim a data.frame")

# Target output -----------------------------------------------------------

correct <- tibble::tribble(
  ~var,     ~type,     ~stat,      ~level,       ~value,
  "weight", "numeric", "missing",  ".all",       0,
  "weight", "numeric", "complete", ".all",       71,
  "weight", "numeric", "n",        ".all",       71,
  "weight", "numeric", "mean",     ".all",       mean(chickwts$weight),
  "weight", "numeric", "sd",       ".all",       sd(chickwts$weight),
  "weight", "numeric", "min",      ".all",       108.000,
  "weight", "numeric", "median",   ".all",       258.000,
  "weight", "numeric", "quantile",  "25%",       204.500,
  "weight", "numeric", "quantile",  "75%",       323.500,
  "weight", "numeric", "max",      ".all",       423.000,
  "weight", "numeric", "hist", "▂▇▂▇▇▃▇▆▂▂", 0.000,
  "feed",   "factor",  "missing",   ".all",      0.0000,
  "feed",   "factor",  "complete",  ".all",      71.0000,
  "feed",   "factor",  "n",         ".all",      71.0000,
  "feed",   "factor",  "count",     "casein",    12.0000,
  "feed",   "factor",  "count",     "horsebean", 10.0000,
  "feed",   "factor",  "count",     "linseed",   12.0000,
  "feed",   "factor",  "count",     "meatmeal",  11.0000,
  "feed",   "factor",  "count",     "soybean",   14.0000,
  "feed",   "factor",  "count",     "sunflower", 12.0000,
  "feed",   "factor",  "count",     NA,      0.0000,
  "feed",   "factor",  "n_unique",  ".all",   6.0000
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

test_that("Skimming a data frame works as expected", {
  input <- skim(chickwts)
  expect_identical(input, correct)
})

test_that("Using skim_tee returns the object", {
  printed_output <- capture.output({ skim_object <- skim_tee(chickwts) })
  expect_identical(chickwts, skim_object)
})

test_that("Using skim_tee prints out the object", {
  printed_output <- capture.output({ skim_object <- skim_tee(chickwts) })
  
  expect_identical(reference_printed_output[1], printed_output[1])
  
})

test_that("Skimming a grouped data frame works as expected", {
  sysfile <- system.file("tests/testthat/skim_output/","group_df.RDS",  package="skimr")
  if (sysfile == ""){
    correct <- readRDS("tests/testthat/skim_output/group_df.RDS")
  } else {
    correct <- readRDS(sysfile)
  }
  input <- mtcars %>% 
    dplyr::group_by(cyl, gear) %>% 
    skim()
  expect_equal(input, correct)
})
