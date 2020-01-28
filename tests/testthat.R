library(testthat)
library(skimr)

options(skimr_strip_metadata = TRUE) # Forcing this for the check
test_check("skimr")
