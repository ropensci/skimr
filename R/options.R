#' Change skim options (Internal)
#' 
#' All skim options are saved within an environment with a single
#' field: current. Options are stored as a named list of lists. The outer list
#' contains data types, e.g. numeric, character, etc. The internal list either
#' has named functions (mean =, median =) or option-value pairs.
#' 
#' @param ... Named arguments that contain named lists
#' @param env A length-one character vector. The set of options to modify,
#'  either functions or formats.
#' @param append Whether the provided options should be in addition to the
#'   defaults already in skim for the given types specified by the named
#'   arugments in \code{...}.
#' @noRd

skim_options <- function(..., env, append) {
  opts <- list(...)
  if (any(is.null(names(opts))) || any(names(opts) == "")) {
    stop("Please used named arguments as follows: <type> = <named list>")
  }
  nms <- purrr::map(opts, names)
  has_null <- purrr::map_lgl(nms, ~any(.x == ""))
  if (any(has_null)) {
    msg <- paste(names(opts)[has_null], collapse = ", ")
    stop("A field is missing a name within this type: ", msg)
  }
  
  purrr::map2(names(opts), opts, set_options, env, append)
  invisible(NULL)
}


#' Set skim options (Internal)
#' 
#' This function is used to set new values for functions or formatting
#' options. 
#'
#' @param type A length-one character vector specifying the type to modify
#' @param newopts New options to assign
#' @param env The environment to modify, either functions or formats
#' @param append Whether the provided options should be in addition to the
#'   defaults already in skim for the given types specified by the named
#'   arugments in \code{...}.
#' @noRd

set_options <- function(type, newopts, env, append) {
  if (!(type %in% names(options[[env]]))) message("Adding type: ", type)
  if (append) {
    nms <- names(newopts)
    options[[env]][[type]][nms] <- unlist(newopts)
  } else {
    options[[env]][[type]] <- newopts
  }
}


#' Show currently assigned skim options
#' 
#' For skimming functions, we show just their names. The function bodies
#' can be confusing. For formatting options, we show everything.
#' 
#' @param which A character vector. One or more of the classes whose summary
#'  functions/ formatting options you wish to display.
#' @param env An option environment to access. One of "functions" or "formats".
#' 
#' @return A list. The names of the list match the classes that have assigned
#'  summary functions or formatting options.
#' @noRd

show_options <- function(which = NULL, env = c("functions", "formats"),
                         only_names = FALSE) {
  stopifnot(is.null(which) || is.character(which),
            is.logical(only_names), length(only_names) == 1)
  env <- match.arg(env)
  if (only_names) {
    opts <- lapply(options[[env]], names)
  } else {
    opts <- options[[env]]
  }
  if (is.null(which)) {
    opts
  } else {
    out <- opts[which]
    missed <- is.na(names(out))
    if (any(missed)) {
      warning("Skim ", env, " aren't defined for type(s): ",
              paste(which[missed], collapse = ", "))
    }
    out[!missed]
  }
}
