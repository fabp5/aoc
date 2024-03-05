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

# split grids into matrices
grids <- map(1:num_grids, ~ filter(grids, pattern == .x) %>% select(starts_with("x_"))) %>%
  map(., ~ janitor::remove_empty(.x, "cols")) %>% 
  map(., as.matrix) %>%
  map(., unname)

# find reflection point horizontally or vertically
find_reflection_hor <- function(grid) {
  
  # for each horizontal line, see if top/bottom is mirrored
  gridheight <- nrow(grid)
  
  for(i in 1:(gridheight-1)){
    cutpt <- i
    
    before_cut <- grid[1:cutpt, , drop = FALSE]
    after_cut <- grid[(cutpt+1):gridheight, , drop = FALSE]
    
    # make same height
    ht <- min(nrow(before_cut), nrow(after_cut))
    
    before_cut <- tail(before_cut, n = ht)
    after_cut <- head(after_cut, n = ht)
    
    # reverse the one after the cut
    after_cut <- after_cut[nrow(after_cut):1, , drop = FALSE]
    
    if(all(after_cut == before_cut)){
      return(cutpt)
    }
  }
  return(0)
}

find_reflection_ver <- function(grid) {
  
  # for each vertical line, see if left/right is mirrored
  gridwidth <- ncol(grid)
  
  for(i in 1:(gridwidth-1)){
    cutpt <- i
    
    before_cut <- grid[ , 1:cutpt, drop = FALSE]
    after_cut <- grid[ , (cutpt+1):gridwidth, drop = FALSE]
    
    # make same width
    wd <- min(ncol(before_cut), ncol(after_cut))
    
    before_cut <- before_cut[, (ncol(before_cut)-wd+1):ncol(before_cut), drop = FALSE]
    after_cut <- after_cut[, 1:wd, drop=FALSE]
    
    # reverse the one after the cut
    after_cut <- after_cut[, ncol(after_cut):1, drop = FALSE]
    
    if(all(after_cut == before_cut)){
      return(cutpt)
    }
  }
  return(0)
}

# get grid score
calculate_grid_score <- function(grid) {
  return(find_reflection_ver(grid) + find_reflection_hor(grid)*100)
}

# sum all grid scores
sum(map_vec(grids, calculate_grid_score))

# part 2 ------------------------------------------------------------------------------------------------

# find the point where one change would cause reflection

# find reflection point
find_almost_reflection_hor <- function(grid) {
  
  gridheight <- nrow(grid)
  
  for(i in 1:(gridheight-1)){
    cutpt <- i
    
    before_cut <- grid[1:cutpt, , drop = FALSE]
    after_cut <- grid[(cutpt+1):gridheight, , drop = FALSE]
    
    # make same height
    ht <- min(nrow(before_cut), nrow(after_cut))
    
    before_cut <- tail(before_cut, n = ht)
    after_cut <- head(after_cut, n = ht)
    
    # reverse the one after the cut
    after_cut <- after_cut[nrow(after_cut):1, , drop = FALSE]
    
    if(sum(!before_cut == after_cut) == 1) {
      return(cutpt)
    }
  }
  return(0)
}

find_almost_reflection_ver <- function(grid) {
  
  gridwidth <- ncol(grid)
  
  for(i in 1:(gridwidth-1)){
    cutpt <- i
    
    before_cut <- grid[ , 1:cutpt, drop = FALSE]
    after_cut <- grid[ , (cutpt+1):gridwidth, drop = FALSE]
    
    # make same width
    wd <- min(ncol(before_cut), ncol(after_cut))
    
    before_cut <- before_cut[, (ncol(before_cut)-wd+1):ncol(before_cut), drop = FALSE]
    after_cut <- after_cut[, 1:wd, drop=FALSE]
    
    # reverse the one after the cut
    after_cut <- after_cut[, ncol(after_cut):1, drop = FALSE]
    
    if(sum(!before_cut == after_cut) == 1) {
      return(cutpt)
    }
  }
  return(0)
}

calculate_grid_score_p2 <- function(grid) {
  return(find_almost_reflection_ver(grid) + find_almost_reflection_hor(grid)*100)
}

# sum all grid scores
sum(map_vec(grids, calculate_grid_score_p2))