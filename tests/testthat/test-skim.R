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

# Begin tests -------------------------------------------------------------

test_that("Skimming a data frame works as expected", {
  input <- skim(chickwts)
  expect_identical(input, correct)
})
