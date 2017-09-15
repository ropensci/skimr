#' Skim a data frame
#'
#' This package provides an alternative to the default summary functions
#' within R. The package's API is tidy, functions take data frames, return
#' data frames and can work as part of a pipeline. The returned \code{skimr}
#' object is subsettable and offers a human readable output.
#' 
#' \code{skimr} is opinionated, providing a strong set of summary statistics
#' that are generated for a variety of differnt data types. It is also
#' provides an API for customization. Users can change both the functions
#' dispatched and the way the results are formatted.
#' 
#' @name skimr-package
#' @aliases skimr
#' @docType package
#' @import stats

NULL

#' @importFrom magrittr %>%
#' @export

magrittr::`%>%`

# Create global package options
options <- new.env()
