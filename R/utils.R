# Adapted from
# https://stackoverflow.com/questions/28248457

fix_unicode <- function(char) {
  m <- gregexpr("<U\\+[0-9a-fA-F]{4}>", char)
  codes <- regmatches(char, m)
  chars <- lapply(codes, make_utf8)
  regmatches(char, m) <- chars
  enc2utf8(char)
}

make_utf8 <- function(x) {
  if (length(x) < 1) {
    return(x)
  } else {
    digits <- substring(x, 4, 7)
    integers <- strtoi(sprintf("0x%s", digits))
    purrr::map_chr(integers, intToUtf8)
  }
}

is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}
