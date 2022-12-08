# day 6: tuning trouble
require(tidyverse)

input <- read_file("06/input.txt")
signal <- gsub("\n$","",input)

# create functions
detect_start_signal <- function(signal, start, window){
  return(n_distinct(str_split_1(substr(signal,start,start+(window-1)), "")) == window)
}

get_chars_processed <- function(signal, window){
  x <- c()
  for (i in 1:nchar(signal)) {
    x[i] <- detect_start_signal(signal, i, window)
  }
  return(detect_index(x, isTRUE) + (window-1))
}

# -------- part 1 --------
get_chars_processed(signal, 4)

# -------- part 2 --------
get_chars_processed(signal, 14)