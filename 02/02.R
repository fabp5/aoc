# day 2: rock paper scissors
library(tidyverse)

example_input <- read_csv("02/example_input.txt", 
                          col_names = FALSE)

input <- read_csv("02/input.txt", 
                  col_names = FALSE)

# make a lookup of RPS outcomes and scores
lookup <- tibble(
  op      = c("R", "R", "R", "P", "P", "P", "S", "S", "S"),
  me      = c("R", "P", "S", "R", "P", "S", "R", "P", "S"),
  outcome = c("D", "W", "L", "L", "D", "W", "W", "L", "D")) %>%
  mutate(score_outcome = case_when(outcome == "W" ~ 6,
                                   outcome == "D" ~ 3,
                                   outcome == "L" ~ 0),
         score_shape = case_when(me == "R" ~ 1,
                                 me == "P" ~ 2,
                                 me == "S" ~ 3),
         score_total = score_shape + score_outcome)

# -------- part 1 --------
generate_part1_score <- function(input_data){
  input_data %>%
    separate(X1, c("op", "me"), sep = " ") %>%
    mutate(op = case_when(op == "A" ~ "R",
                          op == "B" ~ "P",
                          op == "C" ~ "S"),
           me = case_when(me == "X" ~ "R",
                          me == "Y" ~ "P",
                          me == "Z" ~ "S")) %>%
    left_join(., lookup, by = c("op", "me")) %>%
    pull(score_total) %>%
    sum()
}

generate_part1_score(example_input)
generate_part1_score(input)

# -------- part 2 --------
generate_part2_score <- function(input_data){
  input_data %>%
    separate(X1, c("op", "outcome"), sep = " ") %>%
    mutate(op = case_when(op == "A" ~ "R",
                          op == "B" ~ "P",
                          op == "C" ~ "S"),
           outcome = case_when(outcome == "X" ~ "L",
                               outcome == "Y" ~ "D",
                               outcome == "Z" ~ "W")) %>%
    left_join(., lookup, by = c("op", "outcome")) %>%
    pull(score_total) %>%
    sum()
}

generate_part2_score(example_input)
generate_part2_score(input)