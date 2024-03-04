# day 14: rolling rocks ---------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
rockgrid <- read_lines("2023/14/input.txt") %>%
  str_split("") %>%
  do.call(rbind, .)

# roll a line of rocks
sort_rock_line <- function(str) {
  str %>%
    paste(collapse = "") %>%
    # split the string before or after square rock
    str_split_1(., "(?<=#)|(?=#)") %>%
    # split individual vectors
    str_split(., "") %>%
    # convert to factors and sort
    map(.,
        ~factor(., levels = c("O", ".", "#"))) %>%
    map(., sort) %>%
    # paste back together
    unlist() %>%
    as.character()
}

# roll the grid
rockgrid_rolled <- rockgrid %>%
  apply(., 2, sort_rock_line)

# calculate the load
rockgrid_rolled %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(rocks_per_row = sum(c_across(everything()) == "O")) %>%
  ungroup() %>%
  mutate(load_per_rock = nrow(.) - row_number() + 1,
         load = load_per_rock * rocks_per_row) %>%
  pull(load) %>%
  sum()

# part 2 ------------------------------------------------------------------

# roll a line of rocks with specified direction
sort_rock_line_dir <- function(str, down = FALSE) {
  str %>%
    paste(collapse = "") %>%
    # split the string before or after square rock
    str_split_1(., "(?<=#)|(?=#)") %>%
    # split individual vectors into characters
    str_split(., "") %>%
    # convert to factors and sort
    map(.,
        ~factor(., levels = c("O", ".", "#"))) %>%
    map(., ~ sort(.x, decreasing = down)) %>%
    # paste back together
    unlist() %>%
    as.character()
}

spin <- function(grid) {
  grid %>%
    # north
    apply(., 2, sort_rock_line_dir, down = FALSE) %>%
    # west
    apply(., 1, sort_rock_line_dir, down = FALSE) %>%
    t() %>%
    # south
    apply(., 2, sort_rock_line_dir, down = TRUE) %>%
    # east
    apply(., 1, sort_rock_line_dir, down = TRUE) %>%
    t()
}

# function to calculate load
calc_load <- function(grid) {
  grid %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(rocks_per_row = sum(c_across(everything()) == "O")) %>%
    ungroup() %>%
    mutate(load_per_rock = nrow(.) - row_number() + 1,
           load = load_per_rock * rocks_per_row) %>%
    pull(load) %>%
    sum()
}

# look for point where rock state starts to cycle
current_grid <- rockgrid
grid_hashes <- c()

for (i in 1:500){
  current_grid <- spin(current_grid)
  current_hash <- rlang::hash(current_grid)
  if(current_hash %in% grid_hashes){
    first_repeat <- i
    cycle_start <- match(current_hash, grid_hashes)
    break()
  }
  grid_hashes[i] <- current_hash
}

get_end_state <- function(cycle_start, cycle_repeat, end_goal) {
  cycle_length <- cycle_repeat - cycle_start
  offset <- cycle_start - 1
  print(paste0("cycle_length: ", cycle_length))
  end_state <- (end_goal - offset) %% cycle_length + offset
  return(end_state)
}

# find the first grid that will be equivalence to the end state
endstate <- get_end_state(cycle_start, first_repeat, 10^9)

# run the spin cycle to this grid and calculate the load
final_grid <- reduce(1:endstate, ~ spin(.x), .init = rockgrid)
calc_load(final_grid)