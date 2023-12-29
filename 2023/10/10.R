# day 10: pipe maze -------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input_raw <- read_csv("2023/10/input.txt", 
                      col_names = "x")

input <- input_raw %>%
  mutate(x = map(x, ~ str_split_1(.x, ""))) %>%
  unnest_wider(x, names_sep = "")

pipes <- as.matrix(input)

# define the pipe connections
pipe_types <- tibble(pipe = c("|", "-", "L", "J", "7", "F", ".", "S"),
                     type = c("pipe", "pipe", "pipe", "pipe", "pipe", "pipe", "ground", "start"),
                     dirs = list(c("U","D"), c("L", "R"), c("U", "R"), c("U", "L"), c("D", "L"), c("D", "R"), NA, NA))


# function to find the connecting pipes
find_next_tiles <- function(last_tile, current_tile) {
  
  # get pipe type
  cur_pipe <- pipes[current_tile[1],current_tile[2]]
  #print(cur_pipe)
  if(cur_pipe == "."){return(NA)}
  
  # get directions from pipe type
  next_dirs <- unlist(pipe_types$dirs[pipe_types$pipe == cur_pipe])
  
  next_sqs <- list()
  # for each direction, get the next squares
  for(j in 1:2){
    row_diff <- case_match(next_dirs[j],
                           "U" ~ -1,
                           "D" ~ 1,
                           .default = 0)
    col_diff <- case_match(next_dirs[j],
                           "L" ~ -1,
                           "R" ~ 1,
                           .default = 0)
    next_sqs[[j]] <- c(current_tile[1]+row_diff, current_tile[2]+col_diff)
  }
  
  # remove the tile we just came from
  next_sqs <- next_sqs[-which(unlist(map(next_sqs, ~ paste0(.x, collapse = "|"))) == paste0(last_tile, collapse = "|"))]
  next_sqs <- unlist(next_sqs)
  
  return(list(current_tile, next_sqs))
}

# set tiles already seen - here include start tile
start_row <- which(str_detect(input_raw$x, "S"))
start_col <- str_locate(input_raw$x[start_row], "S")[1]
start_tile = c(start_row, start_col)

# possible next tiles
poss_next_tiles <- list(U = c(start_tile[1]-1, start_tile[2]),
                        D = c(start_tile[1]+1, start_tile[2]),
                        L = c(start_tile[1], start_tile[2]-1),
                        R = c(start_tile[1], start_tile[2]+1))

# determine which is a possible path tile
determine_possible_second_tile <- function(dir, rowcol) {
  
  if(0 %in% rowcol){return(NULL)}
  allowable_types <- pipe_types %>%
    unnest_longer(dirs) %>%
    filter(dirs == dir) %>%
    pull(pipe)
  
  if(pipes[rowcol[1],rowcol[2]] %in% allowable_types){
    return(rowcol)
  } else {return(NULL)}
}

second_tiles <- list(determine_possible_second_tile("D", poss_next_tiles[[1]]),
                     determine_possible_second_tile("U", poss_next_tiles[[2]]),
                     determine_possible_second_tile("R", poss_next_tiles[[3]]),
                     determine_possible_second_tile("L", poss_next_tiles[[4]])
) %>%
  compact() # remove null elements

# check 2 tiles are left
if(length(second_tiles) != 2){warning("List of potential second tiles should have length 2")}

# choose first element as second tile
last_tile <- start_tile
current_tile <- second_tiles[[1]]

# make list of tiles seen
tiles_seen <- list()
tiles_seen <- vector("list", 15000)
tiles_seen[[1]] <- start_tile

i <- 1
while(!all(current_tile == start_tile)){
  tiles_seen[[i+1]] <- current_tile
  
  last_tile_t <- find_next_tiles(last_tile, current_tile)[[1]]
  current_tile_t <- find_next_tiles(last_tile, current_tile)[[2]]
  last_tile <- last_tile_t
  current_tile <- current_tile_t
  
  i <- i+1
}
print(paste0("steps: ", i))
print(paste0("steps to furthest point: ", i/2))

# part 2 ------------------------------------------------------------------
# finding the inside area
# we have the path of the loop: keep the loop tiles, and remove everything else

tiles_seen <- tiles_seen %>%
  discard(is.null)

# make empty matrix, add back the pipes in the loop
pipes2 <- matrix(rep(".", nrow(input)*length(input)), nrow(input), length(input))

for (i in 1:length(tiles_seen)){
  pipes2[tiles_seen[[i]][1], tiles_seen[[i]][2]] <- pipes[tiles_seen[[i]][1], tiles_seen[[i]][2]]
}

# function to count the ground tiles in a range
count_ground_tiles_in_range <- function(cur_row, range) {
  tiles_to_count <- cur_row[range[1]:range[2]]
  return(length(tiles_to_count[tiles_to_count == "."]))
}

# function to count the ground tiles in a whole row
count_ground_tiles_in_row <- function(row_num){
  
  # remove chains of pipes that do not cause inside/outside boundary to be crossed
  # replace chains of pipes that cause boundary to be crossed with a single vertical pipe
  cur_row <- pipes2[row_num,] %>%
    paste0(., collapse = "") %>%
    str_replace_all(., "F[-]*7", "") %>%
    str_replace_all(., "L[-]*J", "") %>%
    str_replace_all(., "L[-]*7", "|") %>%
    str_replace_all(., "F[-]*J", "|") %>% 
    str_split_1(., "")
  
  vert_indexes <- which(cur_row == "|")
  if(length(vert_indexes) == 0){return(0)}
  
  # check the number of vertical indexes is even
  if(length(vert_indexes) %% 2 != 0){
    warning("non-even number of vert indexes")
  }
  
  # set "inside" ranges and count ground tiles present
  vert_ranges <- split(vert_indexes, ceiling(seq_along(vert_indexes)/2))
  ground_count <- c()
  for (i in 1:length(vert_ranges)){
    ground_count[i] <- count_ground_tiles_in_range(cur_row, vert_ranges[[i]])
  }
  sum(ground_count) %>% return()
}

# manually change start tile "S" to its tile type
pipes2[pipes2=="S"] <- "7"

# count the inside tiles in each row, and sum up
map_vec(1:nrow(pipes2), ~ count_ground_tiles_in_row(.x)) %>%
  sum()