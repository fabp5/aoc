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

input <- example_input

# split the input into the two parts
stacks <- input %>%
  filter(!grepl("^move", X1))

moves <- input %>%
  filter(grepl("^move", X1))

# sort out the stacks
# reverse the rows
stacks <- map_df(stacks, rev)

# separate into cols
name_stacks <- gsub(" ","", stacks$X1[[1]]) %>%
  strsplit(., "") %>%
  unlist() %>%
  paste0("s", .)
  
# get number of stacks
num_stacks <- length(name_stacks)

stacks <- stacks %>%
  separate(X1,
           into = name_stacks,
           sep = seq(from = 4, to = 4*(num_stacks-1), by = 4)
  ) %>%
  slice(-1) %>% 
  mutate(across(everything(), str_trim),
         across(everything(), ~gsub("\\[|\\]", "", .)),
           across(everything(), ~na_if(., "")))

# rows to a list
stacks <- as.list(stacks)

# remove the NAs
stacks <- map(stacks, ~ as.character(na.omit(.x)))

# look at the moves
moves <- moves %>%
  mutate(X1 = gsub("move ","",X1)) %>%
  separate(X1, into = c("repeat", "from", "to"), sep = " from | to ") %>%
  mutate(across(everything(), as.numeric))

# make a mover function
move_crate <- function(stax, from_stack, to_stack){
  
  crate <- tail(stax[[from_stack]], 1)
  
  stax[[from_stack]] <- head(stax[[from_stack]], -1)
  
  stax[[to_stack]] <- append(stax[[to_stack]], crate)
 
  return(stax)
}

move_crate_sequence <- function(reps, from, to) {
  
  for(i in 1:reps) {
    stacks <- move_crate(stacks, from, to)
  }
 
  return(stacks) 
}


for (i in 1:nrow(moves)){
  stacks <- move_crate_sequence(reps = moves[[1]][i],
                      from = moves[[2]][i],
                      to   = moves[[3]][i])
}

map(stacks, ~ tail(.x, 1)) %>% unlist() %>% paste(collapse = "")
