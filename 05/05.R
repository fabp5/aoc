# day 5: supply stacks
require(tidyverse)

example_input <- read_csv("05/example_input.txt",
                          #skip_empty_rows = FALSE,
                          col_names = FALSE,
                          trim_ws = FALSE
                          )

input <- read_csv("05/input.txt",
                  #skip_empty_rows = FALSE,
                  col_names = FALSE,
                  trim_ws = FALSE
)

# create the stacks data
stacks <- input %>%
  filter(!grepl("^move", X1)) %>%
  map_df(., rev)

# get name and number of stacks
name_stacks <- gsub(" ","", stacks$X1[[1]]) %>%
  strsplit(., "") %>%
  unlist() %>%
  paste0("s", .)

num_stacks <- length(name_stacks)

stacks <- stacks %>%
  separate(X1,
           into = name_stacks,
           sep = seq(from = 4, to = 4*(num_stacks-1), by = 4)) %>%
  slice(-1) %>% 
  mutate(across(everything(), str_trim),
         across(everything(), ~gsub("\\[|\\]", "", .)),
           across(everything(), ~na_if(., ""))) %>%
  as.list() %>%
  map(., ~ as.character(na.omit(.x)))

# create the moves data
moves <- input %>%
  filter(grepl("^move", X1)) %>%
  mutate(X1 = gsub("move ","",X1)) %>%
  separate(X1, into = c("repeat", "from", "to"), sep = " from | to ") %>%
  mutate(across(everything(), as.numeric))

# -------- part 1 -------

# make a mover function
move_crate <- function(stacks, from_stack, to_stack){
  crate <- tail(stacks[[from_stack]], 1)
  stacks[[from_stack]] <- head(stacks[[from_stack]], -1)
  stacks[[to_stack]] <- append(stacks[[to_stack]], crate)
  return(stacks)
}

move_crate_sequence <- function(stacks, reps, from, to) {
  for(i in 1:reps) {
    stacks <- move_crate(stacks, from, to)
  }
  return(stacks) 
}

stacks_p1 <- stacks

# move the crates
for (i in 1:nrow(moves)){
  stacks_p1 <- move_crate_sequence(stacks = stacks_p1,
                                reps = moves[[1]][i],
                                from = moves[[2]][i],
                                to   = moves[[3]][i])
}

map(stacks_p1, ~ tail(.x, 1)) %>% unlist() %>% paste(collapse = "")

# -------- part 2 --------

# make a mover function to move multiple crates at once

move_crates_together <- function(stacks, from_stack, to_stack, num_crates){
  
  crates <- tail(stacks[[from_stack]], num_crates)
  
  stacks[[from_stack]] <- head(stacks[[from_stack]], -num_crates)
  
  stacks[[to_stack]] <- append(stacks[[to_stack]], crates)
  
  return(stacks)
}

stacks_p2 <- stacks

for (i in 1:nrow(moves)){
  stacks_p2 <- move_crates_together(stacks = stacks_p2,
                                from = moves[[2]][i],
                                to   = moves[[3]][i],
                                num_crates = moves[[1]][i])
}

map(stacks_p2, ~ tail(.x, 1)) %>% unlist() %>% paste(collapse = "")