# day 8: tree house
require(tidyverse)

input <- read_csv("08/input.txt",
                          col_names = FALSE)

trees <- input %>%
  separate(X1,
           into = paste0("c", 1:length(input$X1)),
           sep = 1:(length(input$X1)-1)) %>%
  mutate(across(everything(), ~as.numeric(.x))) %>%
  as.matrix()

grid_width <- ncol(trees)
grid_height <- nrow(trees)

check_tree_visible <- function(trow, tcol){
  tree <- trees[[trow,tcol]]
  # edge trees
  if(tcol == 1 | tcol == grid_width | trow == 1 | trow == grid_height){
    return(TRUE)
    # middle trees
    } else {
    if(all(tree > trees[trow,1:(tcol-1)]) |            # left
       all(tree > trees[trow,(tcol+1):grid_width]) |   # right
       all(tree > trees[1:(trow-1), tcol]) |           # up
       all(tree > trees[(trow+1):grid_height, tcol])   # down
       ) {
      return(TRUE)
    } else {
      return(FALSE) }
  }
}

# calculate visible trees
trees_visibility <- data.frame(matrix(NA, nrow = grid_height, ncol = grid_width))

for (i in 1:grid_height){
  for (j in 1:grid_width){
    trees_visibility[i,j] <- check_tree_visible(i, j)
  }
}

sum(colSums(trees_visibility))

# -------- part 2 --------
calculate_scenic_score <- function(trow, tcol){
  tree <- trees[[trow,tcol]]
  
  if(tcol == 1 | tcol == grid_width | trow == 1 | trow == grid_height){
    return(0) }
  else {
    # above
    above <- trees[1:(trow-1), tcol]
    if(all((above >= tree) == FALSE)){
      dist_up <- length(above >= tree)
    } else {
      dist_up <- trow - max(which(above >= tree))
    }
    # below
    below <- trees[(trow+1):grid_height, tcol]
    if(all((below >= tree) == FALSE)){
    dist_down <- length(below >= tree)
    } else {
    dist_down <- min(which(below >= tree))
    }
    # left
    left <- trees[trow, 1:(tcol-1)]
    if(all((left >= tree) == FALSE)){
      dist_left <- length(left >= tree)
    } else {
      dist_left <- tcol - max(which(left >= tree))
    }
    # right
    right <- trees[trow, (tcol+1):grid_width]
    if(all((right >= tree) == FALSE)){
      dist_right <- length(right >= tree)
    } else {
      dist_right <- min(which(right >= tree))
    }
  }
  return(dist_up * dist_down * dist_left * dist_right)
}

# calculate scores
trees_score <- matrix(NA, nrow = grid_height, ncol = grid_width)

for (i in 1:grid_height){
  for (j in 1:grid_width){
    trees_score[i,j] <- calculate_scenic_score(i, j)
  }
}

max(trees_score)
