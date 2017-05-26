skim_print <- function(x){
  nums <- formatnum(x[x$type %in% c("numeric", "double", "integer"),])
  fcts <- formatfct(x[x$type=="factor",])
  return(list(numeric=nums, factors=fcts))
}

formatnum <- function(x){
  tmp <- x[x$level==".all",]
  tmp <- tmp %>% select(-level) %>% spread(key=stat, value=value)
  tmp1 <- x[x$stat=="quantile",]
  tmp1$stat <- paste(tmp1$level, tmp1$stat)
  tmp1 <- tmp1 %>% select(-level) %>% spread(key=stat, value=value)
  tmp <- left_join(tmp, tmp1, by=c("var", "type"))
  tmp2 <- x[x$stat=="hist",]
  tmp2$value <- tmp2$level
  tmp2 <- tmp2 %>% select(-level) %>% spread(key=stat, value=value)
  tmp <- left_join(tmp, tmp2, by=c("var", "type"))
  return(tmp)
}

formatfct <- function(x){
  tmp <- x[x$level==".all",]
  tmp <- tmp[complete.cases(tmp),]
  tmp <- tmp %>% select(-level) %>% spread(key=stat, value=value)
  tmp1 <- x[x$stat=="count",]

  wide <- data_frame()
  for (i in unique(x$var)){
    tmp2 <- tmp1[tmp1$var==i,]
    tmp2$stat <- paste0(tmp2$level, ": ", tmp2$value)
    wide <- rbind(wide, data_frame(var=i, type="factor", stat=paste0(tmp2$stat, collapse="\n")))
  }
  tmp <- left_join(tmp, wide, by=c("var", "type"))
  
  return(tmp)
}