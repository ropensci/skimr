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


#' Create a contingency table and arrange its levels in descending order
#' 
#' In case of ties, the ordering of results is alphabetical and depends upon the locale. 
#' NA is treated as a ordinary value for sorting.
#' 
#' @param x An object that can be interpreted as a factor (including logical)
#' @return A "table" object, which will be treated as a named numeric vector
#' @export

sorted_count <- function(x) {
  tab <- table(x, useNA = "always")
  out <- stats::setNames(as.integer(tab), names(tab))
  sort(out, decreasing = TRUE)
}

#' Generate inline histogram for numeric variables
#'
#' The character length of the histogram is controlled by the formatting
#' options for character vectors.
#'  
#' @param x A numeric vector.
#' @return A length-one character vector containing the histogram.
#' @export

inline_hist <- function(x) {
  # Handle empty vectors
  if (length(x) < 1) return(structure(" ", class = "spark"))
  
  # Addresses a known bug in cut()
  if (all(x == 0)) x <- x + 1
  hist_dt <- table(cut(x, options$formats$character$width))
  hist_dt <- hist_dt / max(hist_dt)
  structure(spark_bar(hist_dt), class = c("spark", "character"))
}


#' Draw a sparkline bar graph with unicode block characters
#'
#' Rendered using [block elements](https://en.wikipedia.org/wiki/Block_Elements).
#' In most common fixed width fonts these are rendered wider than regular
#' characters which means they are not suitable if you need precise alignment.
#'
#' @param x A numeric vector between 0 and 1
#' @param safe Nominally there are 8 block elements from 1/8 height to full
#'   height (8/8). However, the half-height and full-height blocks appear
#'   to be rendered inconsistently (possibly due to font substitution).
#' @examples
#' \dontrun{
#' x <- seq(0, 1, length = 6)
#' spark_bar(x)
#' spark_bar(sample(x))
#'
#' # This might work if you're lucky
#' spark_bar(seq(0, 1, length = 8), safe = FALSE)
#'
#' spark_bar(c(0, NA, 0.5, NA, 1))
#' }
#' @noRd

spark_bar <- function(x, safe = TRUE) {
  stopifnot(is.numeric(x))
  
  bars <- vapply(0x2581:0x2588, intToUtf8, character(1))
  if (safe) {
    bars <- bars[-c(4, 8)]
  }
  
  factor <- cut(
    x,
    breaks = seq(0, 1, length = length(bars) + 1),
    labels = bars,
    include.lowest = TRUE
  )
  chars <- as.character(factor)
  chars[is.na(chars)] <- bars[length(bars)]
  
  structure(paste0(chars, collapse = ""), class = "spark")
}


#' Calculate the number of blank values in a character vector
#' 
#' A "blank" is equal to "".
#' @param x A vector
#' @return The number of values in the vector equal to ""
#' @export

n_empty <- function(x) {
  empty.strings <- c("")
  sum(x %in% empty.strings)
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


#' Calculate the maximum number of characters within a character vector
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
  un <- x[!is.na(x)]
  un <- unique(un)
  length(un)
}


#' Get the start for a time series without the frequency
#' 
#' @param x A vector of ts data
#' @return Finish time.
#' @export

ts_start <- function(x) {
  stats::start(x)[1]
}


#' Get the finish for a time series without the frequency
#' 
#' @param x A vector of ts data
#' @return Finish time.
#' @export

ts_end <- function(x) {
  stats::end(x)[1]
}


#' Generate inline line graph for time series variables
#' 
#' Sets all data to a standard length, to match character formatting. This is
#' twice the number of characters set in the histogram.
#' 
#' The character length of the linegraph is controlled by the formatting
#' options for character vectors.
#' 
#' @param x A vector
#' @return A length-one character vector containing a line graph.
#' @export

inline_linegraph <- function(x) {
  t <- x[!is.na(x)]
  id <- seq(1, length(t), length.out = 2 * options$formats$character$width)
  normalized <- normalize01(t[floor(id)])
  structure(spark_line(normalized), class = c("spark", "character"))
}

# Rescale data to be between 0 and 1
normalize01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#' Draw a sparkline line graph with Braille characters.
#'
#' @inheritParams spark_bar
#' @examples
#' \dontrun{
#' x <- seq(0, 1, length = 10)
#' spark_line(x)
#' }
#' @noRd

spark_line <- function(x) {
  stopifnot(is.numeric(x))
  
  y <- findInterval(x, seq(0, 1, length = 5), all.inside = TRUE)
  
  ind <- matrix(y, ncol = 2, byrow = TRUE)
  ind[, 2] <- ind[, 2] + 4
  
  chars <- apply(ind, 1, braille)
  structure(paste0(chars, collapse = ""), class = "spark")
}


# https://en.wikipedia.org/wiki/Braille_Patterns
braille <- function(x) {
  # remap to braille sequence
  x <- c(7L, 3L, 2L, 1L, 8L, 6L, 5L, 4L)[x]
  
  raised <- 1:8 %in% x
  binary <- raised * 2 ^ (0:7)
  
  # offset in hex is 2800
  val <- 10240 + sum(raised * 2 ^ (0:7))
  
  intToUtf8(val)
}

#' Get the length of the shortest list in a vector of lists
#' 
#' @param x A vector of list data
#' @return Minimum length.
#' @export

list_lengths_min <- function(x) {
  x <- x[!is.na(x)]
  l <- lengths(x)
  ifelse(length(l) != 0, return(min(l)), return(NA))
}


#' Get the median length of the lists
#' 
#' @param x A vector of list data
#' @return Median length.
#' @export

list_lengths_median <- function(x) {
  x <- x[!is.na(x)]
  l <- lengths(x)
  ifelse(length(l) != 0, return(stats::median(l)), return(NA))
}


#' Get the maximum length of the lists
#' 
#' @param x A vector of list data
#' @return Maximum length.
#' @export

list_lengths_max <- function(x) {
  x <- x[!is.na(x)]
  l <- lengths(x)
  ifelse(length(l) != 0, max(l), NA)
}


#' Get the length of the shortest list in a vector of lists
#' 
#' @param x A vector of list data
#' @return Minimum length.
#' @export

list_min_length <- function(x){
  l <- lengths(x)
  min(l)
}


#' Get the length of the longest list in a vector of lists
#' 
#' @param x A vector of list data
#' @return Minimum length.
#' @export

list_max_length <- function(x){
  l <- lengths(x)
  max(l)
}


