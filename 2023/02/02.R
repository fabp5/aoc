# day 2 -------------------------------------------------------------------------------------------------
library(tidyverse)


# attempt 1: with loops ---------------------------------------------------------------------------------
# part 1 ------------------------------------------------------------------------------------------------
input <- read_lines("2023/02/input.txt")

games <- list(input)[[1]] %>%
  str_replace_all(., "Game.+: ", "") %>%
  str_split(., "; ") %>%
  map(., ~ str_split(.x, ", "))

for (g in 1:length(games)){
  for (r in 1:length(games[[g]])){
    names(games[[g]][[r]]) <- str_extract_all(games[[g]][[r]], "[a-z]+")
    games[[g]][[r]] <- as_tibble(as.list(games[[g]][[r]])) %>%
      mutate(across(everything(), ~ str_replace_all(.x, " [a-z]+", "")))
  }
}

# get colour draws, then max number of each colour in each game
game_maxes <- map(games, bind_rows, .id = "round") %>%
  bind_rows(., .id = "game") %>%
  mutate(across(everything(), ~ as.numeric(replace_na(.x, "0")))) %>%
  group_by(game) %>%
  summarise(max_blue = max(blue),
            max_red = max(red),
            max_green = max(green))

# get the sum of possible games
game_maxes %>%
  filter(max_blue <= 14,
         max_red <= 12,
         max_green <= 13) %>%
  pull(game) %>%
  as.numeric() %>%
  sum()

# part 2 ------------------------------------------------------------------------------------------------
game_maxes %>%
  mutate(power = max_blue*max_red*max_green) %>%
  pull(power) %>%
  sum()

# attempt 2: cleaner, with dataframes -------------------------------------------------------------------
# part 1 ------------------------------------------------------------------------------------------------
game_maxes <- tibble(x = input) %>%
  separate(x, into = c("game", "balls"), sep = ": ") %>%
  mutate(game = as.numeric(str_replace(game, "Game ", ""))) %>%
  separate_longer_delim(balls, "; ") %>%
  mutate(round = row_number()) %>%
  separate_longer_delim(balls, delim = ", ") %>%
  separate(balls, into = c("num", "colour")) %>%
  mutate(num = as.numeric(num)) %>%
  summarise(max = max(num), .by = c(game, colour))

game_maxes %>%
  mutate(possible = (colour == "blue" & max <= 14 |
                       colour == "red" & max <= 12 |
                       colour == "green" & max <= 13)) %>% 
  summarise(possible = all(possible), .by = game) %>%
  filter(possible) %>%
  pull(game) %>%
  sum()

# part 2 ------------------------------------------------------------------------------------------------
game_maxes %>%
  summarise(power = prod(max), .by = game) %>%
  pull(power) %>% sum()
