#' Manages print for skim objects.
#' 
#' Currently, numeric, factor and character data are handled.
#' 
#' @param x A \code{skim_df} object
#' @importFrom dplyr select mutate filter data_frame left_join
#' @importFrom tidyr gather spread
#' @return A \code{skim_df} object
#' @export
skim_print <- function(x){
 
  x$stat <- ifelse(x$level %in% c(".all") | x$stat == "hist" | x$stat == "count", x$stat,  paste(x$stat, x$level))
  return_list <- list()
  types <- unique(x$type)
  types_custom <- c("numeric", "double", "integer", "factor", "character")
  types_other <- subset(types, !types %in% types_custom)
  
  y <- x %>% filter(type %in%  c("numeric", "double", "integer")) 
  if  (nrow(y) > 0){
    order<-unique(y$stat)
    type <- "Numeric"
    histograms <- y %>% dplyr::filter(stat == "hist")
    y <- y %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
    calculated <- c("max",  "mean", "median", "min", "quantile 25%", "quantile 75%", "sd")
    # Not a real solution
    y[calculated] <- round(y[calculated], digits = 2)
    y$hist <- histograms$level
    
    y <- y[c("var", order)]
    return_list[[type]] <- y
    attr(return_list[[type]], "subtibble_title") <- paste(type, "Variables\n")
  }

  y <- x %>% filter(type == "factor")
  if  (nrow(y) > 0){
    type = "Factor"
    counts <- y %>% dplyr::filter (stat == "count") 
    counts <- paste(paste(counts$var, counts$level, counts$value, sep = ":"), collapse = " ")
    
    y <- y %>% dplyr::filter(stat != "count") %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
    y <- tibble::add_column(y, counts)
    return_list[[type]] <- y
    attr(return_list[[type]], "subtibble_title") <- paste(type, "Variables\n")
  }

  y <- x %>% filter(type == "character")
  if  (nrow(y) > 0){
    type = "Character"
    order<-unique(y$stat)
    y <- y %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
    y <- y[c("var", order)]
    return_list[[type]] <- y
    attr(return_list[[type]], "subtibble_title") <- paste(type, "Variables\n")
  }
  
  y <- x %>% filter(type %in% types_other)
  if (nrow(y) > 0) {
    for (i in 1:length(types_other)){
      z <- x %>% filter(type == types_other[i])
        order<-unique(z$stat)
        z <- z %>% dplyr::select(var, stat, value) %>% tidyr::spread( stat, value)
        z <- z[c("var", order)]
        return_list[[types_other[i]]] <- z 
        attr(return_list[[type]], "subtibble_title") <- paste(type, "Variables\n")
    }
  }

  return(return_list)   
}

#' @export
print.skim_df <- function(x, ...) {
  lapply(skim_print(x),print_results)
}

print_results<-function(subtibble){
  cat(attr(subtibble, "subtibble_title"))
  print(subtibble)
}