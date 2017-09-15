#' @include skimr-package.R
NULL

#' Change the formatting options for printed skim objects
#' 
#' Skim uses \code{\link{format}} to convert the numeric values returned by
#' the summary functions into displayed values. The default options are a
#' subset of options available in that function.
#' 
#' Generally speaking, formats are dispatched according to the type of value
#' returned by the "skimmer," i.e. summary function. One special formatting
#' "type" exists for the names of the returned vector. The names are used
#' to assign the levels for statistics that have more than one value. Counts
#' and quantiles are common cases.
#' 
#' When a vector is named, the name and the value are combined into a single
#' formatted value. To deal with excessively long names for factor levels,
#' only the first three characters of the name are returned by default. This
#' can be changed by setting a new value for \code{max_char} within the
#' \code{.levels} type.
#' 
#' @param ... Named arguments that contain named lists specifying formats
#'  to apply.
#' @param append Whether the provided options should be in addition to the
#'  defaults already in skim for the given types specified by the named
#'  arugments in \code{...}.
#' @return Nothing. \code{invisible(NULL)}
#' @examples
#' # Format numbers to have more digits
#' skim_format(numeric = list(digits = 3))
#' 
#' # Show 4-character names in factor levels
#' skim_format(.levels = list(nchar = 4))
#' 
#' # Reset to the defaults
#' skim_format_defaults()
#' @export

skim_format <- function(..., append = TRUE) {
  skim_options(..., env = "formats", append = append)
}


#' @describeIn skim_format Use the default formatting options within skim
#' @export

skim_format_defaults <- function() {
  assign("formats", .formats, envir = options)
}


#' Show formatting options currently used, by data type
#' 
#' @param which A character vector. One or more of the classes whose formatting
#'  options you wish to display.
#' @return A list of option-value pairs.
#' @export

show_formats <- function(which = NULL) {
  show_options(which, "formats")
}

.formats <- list(
  .levels = list(max_char = 3, max_levels = 4),
  numeric = list(digits = 2, nsmall = 2),
  integer = list(drop0trailing = TRUE),
  character = list(width = 10),
  date = list(format = "%Y-%m-%d"),
  posixct = list(format = "%Y-%m-%d"),
  logical = list(),
  asis = list()
)

# Set the default formatting options
options$formats <- .formats

#' Internal functions for generating formatted versions of summary
#' statistics. Generally speaking, formats are dispatched according to the
#' value that is returned by the "skimmer," i.e. formatting function.
#'
#' The existence of levels makes this a little more complicated. We check for
#' them by looking at a vector's length and create the formatted values using
#' the vector's names. If the vector is only length one, we don't care whether
#' or not it's named.
#' 
#' @param x A vector of computed statistics to format.
#' @return A length-one character vector that contains a formatted version of
#'  the statistic. This is the verion that should be ready for printing.
#' @noRd

get_formatted <- function(x) {
  formats <- get_formats(class(x))
  if (length(x) > 1) {
    formatted <- purrr::map(x, get_formatted)
    trimmed <- substr(names(x), 1, options$formats$.levels$max_char)
    paste(trimmed, trimws(formatted), sep = ": ")
  } else {
    do.call(format, c(x = list(unname(x)), formats))
  }
}


#' Get the formatting options of a particular type (Internal)
#' 
#' @param type A length-one character vector
#' @return A list of formatting options
#' @noRd
 
get_formats <- function(type) {
  low <- tolower(type)
  id <- purrr::detect_index(low, ~.x %in% names(options$formats))
  if (id) {
    options$formats[[low[id]]]
  } else {
    warning("Skimr does not know how to format type: ",
            paste(type, collapse = ", "), ". Leaving as is.")
    list()
  }
}
