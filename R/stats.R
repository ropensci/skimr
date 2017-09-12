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


#' Calculate mean and create formatted value
#' 
#' Always excludes missings
#' @param x A numeric vector
#' @return The mean and the formatted value
#' @export
mean_num<- function(x){
  mean <- mean(x, na.rm = TRUE)
  attr(mean, "formatted_value") <- formatC(mean, width = 5, digits = 1,  format = "f", flag ="#")

  mean
}

#' Calculate standard deviation and create formatted value
#' 
#' Always excludes missings
#' @param x A numeric vector
#' @return The mean and the formatted value
#' @export
sd_num<- function(x){
  sd <- stats::sd(x, na.rm = TRUE)
  attr(sd, "formatted_value") <- formatC(sd,  width = 5, digits = 1,  format = "f", flag ="#")

  sd
}

#' Calculate median and create formatted value
#' 
#' Always excludes missings
#' @param x A numeric vector
#' @return The mean and the formatted value
#' @export
median_num<- function(x){
  med <- stats::median(x, na.rm = TRUE)
  attr(med, "formatted_value") <- formatC(med,  width = 5, digits = 1,  format = "f", flag ="#")

  med
}

#' Calculate 25th and 75th quantiles and create formatted value
#' 
#' Always excludes missings
#' @param x A numeric vector
#' @return The 25th and 75th and formatted values
#' @export
quantile_num<- function(x){
  quantiles <- stats::quantile(x, probs = c(.25, .75), na.rm = TRUE)
  attr(quantiles, "formatted_value") <- c(formatC(quantiles[1],  width = 5, digits = 1,  format = "f", flag ="#"),
                                    formatC(quantiles[2],  width = 5, digits = 1,  format = "f", flag ="#"))
  quantiles
}


#' Generate inline histogram for numeric variables
#' 
#' @param x A vector
#' @return A A numeric value of 0 with a name that is a character string of histogram.
#' @export

inline_hist <- function(x) {
 
  x <- x[!is.na(x) == TRUE]
  out <- 0
  if (length(x) == 0 | all(x == 0)){
    attr(out, "formatted_value") <- ""
    return(out)
  }
  hist_dt <- table(cut(x, 10))
  hist_dt <- hist_dt / max(hist_dt)
  attr(out, "formatted_value") <- pillar::spark_bar(hist_dt)

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
  s <- stats::start(x)
  s <- s[1]
}

#' Get the finish for a time series without the frequency
#' 
#' @param x A vector of ts data
#' @return Finish time.
#' @export

ts_end <- function(x) {
  e <- stats::end(x)
  e <- e[1]
}


#' Generate inline line graph for time series variables
#' 
#' @param x A vector
#' @return A numeric value of 0 with a name that is a character string of line gragh.
#' @export

inline_linegraph <- function(x) {
  t <- x[!is.na(x)]
  out <- 0 
  if (length(t) == 0 ){
    attr(out, "formatted_value") <- ""
    return(out)    
  }
  if (length(t) > 39){
    shrink_factor <-ceiling(length(t)/40)
    t <- t[seq(1, length(t), shrink_factor) ]
  }

  # Values must be between 0 and 1.
  t <- (t - min(t))/(max(t) - min(t))

  attr(out, "formatted_value") <- suppressWarnings(pillar::spark_line(t))

  return(out)

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
  return(stats::median(l))

}

#' Get the maximum length of the lists
#' 
#' @param x A vector of list data
#' @return Maximum length.
#' @export

list_lengths_max <- function(x) {
  x <- x[!is.na(x)]
  l <- lengths(x)
  ifelse(length(l) != 0, return(max(l)), return(NA))
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

#' Get the maximum in a vector of dates
#' 
#' @param x A vector of date data
#' @return Maximum date.
#' @export
date_max <- function(x){
  d <- max(x, na.rm = TRUE)
  d_char <- as.character(d)
  attr(d, "formatted_value") <- d_char
  
  d
}

#' Get the minimum in a vector of dates
#' 
#' @param x A vector of date data
#' @return Maximum date.
#' @export
date_min <- function(x){
  d <- min(x, na.rm = TRUE)
  d_char <- as.character(d)
  attr(d, "formatted_value") <- d_char
  d
}

#' Get the median in a vector of dates
#' 
#' @param x A vector of date data
#' @return Maximum date.
#' @export
date_median <- function(x){
  d <- stats::median(x, na.rm = TRUE)
  d_char <- as.character(d)
  attr(d, "formatted_value") <- d_char
  d
}

#' Get the maximum in a vector of POSIXct dates
#' 
#' @param x A vector of date data
#' @return Maximum date.
#' @export
posixct_max <- function(x){
  d <- max(x, na.rm = TRUE)
  d_char <- as.character(d)
  attr(d, "formatted_value") <- d_char
  d
}

#' Get the minimum in a vector of POSIXct dates
#' 
#' @param x A vector of date data
#' @return Maximum date.
#' @export
posixct_min <- function(x){
  d <- min(x, na.rm = TRUE)
  d_char <- as.character(d)
  attr(d, "formatted_value") <- d_char
  d
}

#' Get the median in a vector of POSIXct dates
#' 
#' @param x A vector of date data
#' @return Maximum date.
#' @export
posixct_median <- function(x){
  d <- stats::median(x, na.rm = TRUE)
  d_char <- as.character(d)
  attr(d, "formatted_value") <- d_char
  d
}
