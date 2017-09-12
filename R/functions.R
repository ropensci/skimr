#' @include defaults.R
NULL

# Build environment for storing functions ---------------------------------

functions <- new.env()
functions$default <- .default
functions$current <- .default


#' Set or add the summary functions for a particular type of data
#' 
#' While skim is designed around having an opinionated set of defaults, you
#' can use this function to change the summary statistics that it returns.
#' To do that, provide type you wish to change as an argument to this function,
#' along with a list of named functions that you want to use instead of the
#' defaults. The \code{append} argument lets you decide whether you want to
#' replace the defaults or add to them.
#' 
#' This function is not pure. It sets values within the package environment.
#' This is an intentional design choice, with effects similar to setting
#' options in base R. By setting options here for your entire session, you
#' can continue to summarize using skim on its own.
#' 
#' @param ... A list of functions, with an argument name that matches a
#'   particular data type.
#' @param append Whether the provided functions should be in addition to the
#'   defaults already in skim.
#' @return Nothing. \code{invisible(NULL)}
#' @export
#' @examples
#' \dontrun{
#' # Use new functions for numeric functions
#' skim_with(numeric = list(median = median, mad = mad), append = FALSE)
#' skim(faithful)
#' 
#' # Go back to defaults
#' skim_with_defaults()
#' skim(faithful)
#' 
#' # If you want to remove a particular skimmer, set it to NULL
#' # This removes the inline histogram
#' skim_with(numeric = list(hist = NULL))
#' skim(faithful)
#' }

skim_with <- function(..., append = TRUE) {
  # Argument assertions
  stopifnot(is.logical(append), length(append) == 1)
  funs <- list(...)

  if (any(is.null(names(funs))) || any(names(funs) == "")) {
    stop("Please used named arguments as follows: <type> = <list of functions>")
  }
  nms <- purrr::map(funs, names)

  purrr::map2(names(funs), funs, set_functions, append)
  invisible(NULL)
}

set_functions <- function(type, newfuns, append) {
  exists <- type %in% names(functions$current)
  
  if (!exists) {
    message("Adding new summary functions for type: ", type)
  } else if (append) {
    old <- functions$current[[type]]
    for (fn in names(newfuns)) {
      old[[fn]] <- newfuns[[fn]]
    }
    newfuns <- old
  }
  
  functions$current[[type]] <- newfuns
}


#' @describeIn skim_with Use the default functions within skim
#' @export

skim_with_defaults <- function() {
  assign("current", .default, envir = functions)
}


#' Show summary functions currently used, by column type
#' 
#' The names of the summary functions are stored as a character vector within
#' a list. The names of the values in the list match the names of the classes
#' that have assigned sets of summary functions.
#' 
#' @param selected_classes A character vector. One or more of the classes whose summary
#'  functions you wish to display.
#' @return A list. The names of the list match the classes that have assigned
#'  summary functions.
#' @export

show_skimmers <- function(selected_classes = NULL) {
  stopifnot(is.null(selected_classes) || is.character(selected_classes))
  skimmers <- lapply(functions$current, names)
  if (is.null(selected_classes)) {
    skimmers
  } else {
    out <- skimmers[selected_classes]
    missed <- is.na(names(out))
    if (any(missed)) {
      warning("Skim functions aren't defined for type(s): ",
             paste(selected_classes[missed], collapse = ", "))
    }
    out[!missed]
  }
}


# An internal method for getting a set of summary functions, by type. We use
# this approach instead of method dispatch because we want to be able to
# dynamically add or remove the summary functions for each type.
#
# The call to purrr::detect returns the first appropriate match for objects
# that have more than one class. Doing it this way saves us from having
# to define methods for multiple classes.
#
# @param type The type of summary functions to extract
# @return A list of summary functions
# @keywords internal
# @export

get_funs <- function(type) {
  all <- functions$current[type]
  purrr::detect(all, purrr::compose(`!`, is.null))
}


# A method for getting the name of set of summary functions names, by type.
# This function is similar to get_funs, in that it applies a type of dispatch
# for the type of object provided. This function gets the name of the
# group of summary functions for the type. This lets the user know what set of
# functions are used in producing the skim_df.
#
# @param type The type of summary functions to extract
# @return A length-one character vector that shows the class that was matched
#  by skimr.
# @keywords internal
# @export

get_fun_names <- function(type) {
  all <- functions$current[type]
  id <- purrr::detect_index(all, purrr::compose(`!`, is.null))
  names(all)[id]
}