#' Skim a data frame
#'
#' This package provides an alternative to the default summary functions
#' within R. The package's API is tidy, functions take data frames, return
#' data frames and can work as part of a pipeline. The returned `skimr`
#' object is subsettable and offers a human readable output.
#' 
#' `skimr` is opinionated, providing a strong set of summary statistics
#' that are generated for a variety of different data types. It is also
#' provides an API for customization. Users can change both the functions
#' dispatched and the way the results are formatted.
#' 
#' @name skimr-package
#' @aliases skimr
#' @docType package
NULL


# Imports -----------------------------------------------------------------

#' @importFrom rlang %||%

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom tidyselect contains
#' @aliases select_helpers
#' @export
tidyselect::contains

#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with

#' @importFrom tidyselect everything
#' @export
tidyselect::everything

#' @importFrom tidyselect matches
#' @export
tidyselect::matches

#' @importFrom tidyselect num_range
#' @export
tidyselect::num_range

#' @importFrom tidyselect one_of
#' @export
tidyselect::one_of

#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with

# Silence complaints about NSE.
globalVariables(".")
