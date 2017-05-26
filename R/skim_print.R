formatnum <- function(x){
  tmp <- x[x$level==".all",]
  tmp <- tmp %>% select(-level) %>% spread(key=stat, value=value)
  tmp1 <- x[x$stat=="quantile",]
  tmp1$stat <- paste(tmp1$level, tmp1$stat)
  tmp1 <- tmp1 %>% select(-level) %>% spread(key=stat, value=value)
  tmp <- left_join(tmp, tmp1)
  tmp2 <- x[x$stat=="hist",]
  tmp2$value <- tmp2$level
  tmp2 <- tmp2 %>% select(-level) %>% spread(key=stat, value=value)
  tmp <- left_join(tmp, tmp2)
  return(tmp)
}

formatfct <- function(x){
  tmp <- x[x$level==".all",]
  tmp <- tmp[complete.cases(tmp),]
  tmp <- tmp %>% select(-level) %>% spread(key=stat, value=value)
  tmp$idx <- paste0(tmp$var, "factor")
  tmp1 <- x[x$stat=="count",]

  wide <- data_frame()
  for (i in unique(x$var)){
    tmp2 <- tmp1[tmp1$var==i,]
    first = TRUE
    for (row_i in 1:nrow(tmp2)) {
      row <- tmp2[row_i,]
      if (first) {
        wide <- rbind(wide, data_frame(idx=paste0(i, "factor"), var=i, type="factor", stat=paste0(row[1,]$level, ": ", row[1,]$value)))
      } else {
        # fixme
        wide <- rbind(wide, data_frame(idx=paste0(i, "factor"), var="", type="", stat=paste0(row$level, ": ", row$value)))
      }
      first = FALSE
    }
#    tmp2$stat <- paste0(tmp2$level, ": ", tmp2$value)
#    wide <- rbind(wide, data_frame(var=i, type="factor", stat=paste0(tmp2$stat, collapse="\n")))
  }
  tmp <- left_join(tmp, wide, by=c("idx"))
  tmp$idx <- NULL
  tmp$var.x <- NULL
  tmp$type.x <- NULL
  
  j <- min(which(names(tmp) == "var.y"))
  
  return(tmp)
}