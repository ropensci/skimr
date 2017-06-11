# Summary statistic functions

#' Calculate missing values
#' 
#' @param x A vector
#' @return The sum of NULL and NA values
#' @export

n_missing <- function(x) {
  sum(is.na(x) | is.null(x))
}


#' Calculate complete values
#' 
#' Complete values are not missing
#' @param x A vector
#' @return The sum of non-NULL and non-NA values
#' @export

n_complete <- function(x) {
  length(x) - n_missing(x)
}

#' Generate inline histgram for numeric variables
#' 
#' @param x A vector
#' @return A A numeric value of 0 with a name that is a character string of histogram.
#' @export

inline_hist <- function(x) {
  x <- x[!is.na(x)]
  out <- 0
  if ( !all(x == 0) & length(x) != 0) {
    hist_dt <- table(cut(x, 10))
    hist_dt <- hist_dt / max(hist_dt)
    names(out) <- colformat::spark_bar(hist_dt)
    return(out)
  }
  names(out) <- ""
  return(out)
}


#' Calculate the number of blank values in a character vector
#' 
#' A "blank" is equal to "".
#' @param x A vector
#' @return The number of values in the vector equal to ""
#' @export

n_empty <- function(x) {
  empty.strings=c("")
  x %in% empty.strings %>% sum()
}


#' Calculate the minimum number of characters within a character vector
#' 
#' @param x A vector
#' @return The min of calling nchar(x).
#' @export

min_char <- function(x) {
  characters <- nchar(x)
  min(characters, na.rm = TRUE)
}


#' Calculate the minimum number of characters within a character vector
#' 
#' @param x A vector
#' @return The min of calling nchar(x).
#' @export

max_char <- function(x) {
  characters <- nchar(x)
  max(characters, na.rm = TRUE)
}

#' Calculate the number of unique elements but remove NA
#' 
#' @param x A vector
#' @return unique without NA.
#' @export

n_unique <- function(x) {
  un <- unique(x)
  un[!is.na(un)]
}

#' Get the start for a time series without the frequency
#' 
#' @param x A vector of ts data
#' @return Finish time.
#' @export

ts_start <- function(x) {
  s <- start(x)
  s <- s[1]
}

#' Get the finish for a time series without the frequency
#' 
#' @param x A vector of ts data
#' @return Finish time.
#' @export

ts_end <- function(x) {
  e <- end(x)
  e <- e[1]
}

#' Generate inline line graph for time series variables
#' 
#' @param x A vector
#' @return A numeric value of 0 with a name that is a character string of histogram.
#' @export

inline_linegraph <- function(x) {
  x <- x[!is.na(x)]
  out <- 0 
  if ( length(x) != 0) {
      # Values must be between 0 and 1.
      x <- (x - min(x))/(max(x) - min(x))
      names(out) <- suppressWarnings(colformat::spark_line(x))
      return(out)
    }  
  names(out) <- ""
  return(out)
}