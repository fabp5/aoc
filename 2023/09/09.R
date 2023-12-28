# day 9: oasis ------------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_csv("2023/09/input.txt", 
                          col_names = "x") %>%
  mutate(x = map(x, ~ as.numeric(str_split_1(.x, " "))))

predict_next_num <- function(history) {
  
  diffs_list <- list()
  diffs_list[[1]] <- history
  
  # get diffs until there are no more
  i <- 0
  diffs_equal <- FALSE
  while(!diffs_equal){
    i = i+1
    diffs_list[[i+1]] <- diffs_list[[i]] - lag(diffs_list[[i]])
    diffs_list[[i+1]] <- diffs_list[[i+1]][!is.na(diffs_list[[i+1]])]
    if(all(diffs_list[[i+1]] == 0)){diffs_equal = TRUE}
  }
  
  # add the numbers back up
  return(map_vec(diffs_list, ~ tail(.x, n = 1)) %>% sum())
}

# sum next numbers in all sequences
input %>%
  mutate(next_num = map_vec(x, ~ predict_next_num(.x))) %>%
  pull(next_num) %>% sum()

# part 2 ------------------------------------------------------------------
predict_first_num <- function(history) {
  
  diffs_list <- list()
  diffs_list[[1]] <- history
  
  # get diffs until there are no more
  i <- 0
  diffs_equal <- FALSE
  while(!diffs_equal){
    i = i+1
    
    diffs_list[[i+1]] <- diffs_list[[i]] - lag(diffs_list[[i]])
    diffs_list[[i+1]] <- diffs_list[[i+1]][!is.na(diffs_list[[i+1]])]
    
    if(all(diffs_list[[i+1]] == 0)){diffs_equal = TRUE}
  }
  num_levels <- i+1
  
  first_nums <- map_vec(diffs_list, ~ head(.x, n = 1))
  
  to_subtract <- first_nums[num_levels]
  for(i in 0:(num_levels-2)){
    to_subtract <- first_nums[num_levels-(i+1)] - to_subtract
  }
  return(to_subtract)
}

input %>%
  mutate(first_num = map_vec(x, ~ predict_first_num(.x))) %>%
  pull(first_num) %>% sum()

# attempt 2 ---------------------------------------------------------------
# more simply: reverse input and use part 1 function
input %>%
  mutate(x = map(x, rev)) %>%
  mutate(next_num = map_vec(x, ~ predict_next_num(.x))) %>%
  pull(next_num) %>%
  sum()