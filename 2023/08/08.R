# day 8: haunted wasteland ------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_lines("2023/08/input.txt")

dirs <- input[1]
nodes <- input[-c(1,2)] %>%
  as_tibble() %>%
  separate_wider_delim(value, " = ", names = c("start", "lr")) %>%
  separate_wider_delim(lr, ", ", names = c("L", "R")) %>%
  mutate(across(c(L, R), ~ str_replace_all(.x, "\\(|\\)", "")))

# function to choose path
find_next_node <- function(start_node, direction) {
  nodes %>%
    filter(start == start_node) %>%
    pull({{direction}}) %>%
    return()
}

dirs <- str_split_1(dirs, "")
len_dirs <- length(dirs)

node_to_start_from <- "AAA"
i <- 0
while(node_to_start_from != "ZZZ"){
  i <- i+1
  pos <- ifelse(i %% len_dirs == 0, len_dirs, i %% len_dirs)
  direc <- dirs[pos]
  
  node_to_start_from <- find_next_node(node_to_start_from, direc)
}
print(i)

# part 2 ------------------------------------------------------------------
# get the starting nodes
starting_nodes <- nodes$start[str_detect(nodes$start, "A$")]

# with each starting node, how often do the directions loop and where do Z-ending nodes occur?

# function to find loop length and Z positions
find_loop_and_z <- function(start_node) {
  
  combo_vec <- vector(mode="character", length=100000)
  node_to_start_from <- start_node
  
  loop_found <- FALSE
  i <- 0
  while(!loop_found){
    i <- i+1
    
    pos <- ifelse(i %% len_dirs == 0, len_dirs, i %% len_dirs)
    direc <- dirs[pos]
    node_to_start_from <- find_next_node(node_to_start_from, direc)
    combo <- paste0(node_to_start_from, "-", pos)
    
    if(combo %in% combo_vec){
      loop_found <- TRUE
      combo_vec[i] <- combo
    }
      combo_vec[i] <- combo
  }
  
  # identify start and end of loop
  # remove empty bits
  combo_vec <- combo_vec[combo_vec != ""]
  duplicated_combo <- combo_vec[duplicated(combo_vec)]
  start_end_loop <- which(combo_vec == duplicated_combo)
  loop_length <- start_end_loop[2] - start_end_loop[1]
  
  # identify all the times a Z is found
  z_pos <- which(str_detect(combo_vec, "Z-"))
  
  return(list(ll = loop_length, z = z_pos, cvec = combo_vec))
}

loop_list <- list()

for(i in 1:length(starting_nodes)){
  loop_list[[i]] <- find_loop_and_z(starting_nodes[i])
}

# z positions and loop lengths are the same
# so need only to get lowest common multiple of the loop lengths
Reduce(pracma::Lcm, unlist(map(loop_list, ~ pluck(.x, "z"))))
