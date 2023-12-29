# day 13: mirrors ---------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_lines("2023/13/input.txt")

# split group of patterns at the empty lines
grids <- as_tibble(input) %>%
  mutate(value = na_if(value, '')) %>% 
  mutate(pattern = cumsum(is.na(value))+1) %>% 
  na.omit()

num_grids <- max(grids$pattern)

grids <- grids %>%
  mutate(x = map_vec(value, ~ str_split(.x, ""))) %>%
  unnest_wider(x, names_sep = "_")

# split grids into dfs
grids <- map(1:num_grids, ~ filter(grids, pattern == .x) %>% select(starts_with("x_"))) %>%
  map(., ~ janitor::remove_empty(.x, "cols"))

# find reflection point in a vector
find_reflection_point <- function(vec) {
  
  vec_len <- length(vec)
  reflection_found <- FALSE
  
  for(i in 1:(vec_len-1)){
    cutpt <- i
    
    before_cut <- vec[1:cutpt]
    after_cut <- vec[(cutpt+1):vec_len]
    
    # shorten before/after so they're the same length
    len <- min(length(before_cut), length(after_cut))
    before_cut <- tail(before_cut, len)
    after_cut <- head(after_cut, len)
    
    if(identical(before_cut, rev(after_cut))){
      reflection_found <- TRUE
      break()
      }
  }
  if(reflection_found){
    return(i)
  } else {
    return(0)
  }
}

# get grid score
calculate_grid_score <- function(grid) {
  grid_width <- ncol(grid)
  grid_height <- nrow(grid)
  cols_to_compare <- map_vec(1:grid_width, ~ paste0(grid[[.x]], collapse = ""))
  rows_to_compare <- map_vec(1:grid_height, ~ paste0(grid[.x, ], collapse = ""))
  return(find_reflection_point(cols_to_compare) + find_reflection_point(rows_to_compare)*100)
}

# sum all grid scores
sum(map_vec(grids, calculate_grid_score))