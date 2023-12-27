# day 4 -------------------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_csv("2023/04/input.txt", 
                  col_names = "x")

cards <- input %>%
  separate(x, into = c("card", "nums"), sep = ": ") %>%
  mutate(card = as.numeric(str_extract(card, "\\d+"))) %>%
  separate(nums, into = c("winning", "mine"), sep = " \\| ") %>%
  mutate(across(c(mine, winning), ~ trimws(.x)),
         mine = map(mine, ~ str_split_1(.x, " +")),
         winning = map(winning, ~ str_split_1(.x, " +"))) %>%
  rowwise() %>%
  mutate(num_matched = sum(mine %in% winning),
         score = floor(2^(num_matched-1))) %>%
  ungroup()

# get the sum of the scores
cards %>%
  pull(score) %>%
  sum()

# part 2 ------------------------------------------------------------------
# which cards win which copies?
cards <- cards %>%
  rowwise() %>%
  mutate(cards_won = map2(card + 1, card + num_matched, ~ seq(.x, .y)),
         cards_won = ifelse(num_matched == 0, NA, list(cards_won))) %>%
  select(-c(winning, mine)) %>%
  ungroup()

# set number of cards we begin with
num_cards_start <- nrow(cards)

# create list for cards won per loop and the cards to work with each time
cards_so_far_df <- tibble(card = seq(1:num_cards_start),
                          copies = rep(1, num_cards_start))

# loop through each card
for (i in 1:num_cards_start){

  # how many copies of the current card do we have?
  copies_of_current <- cards_so_far_df[[i,2]]
  
  # which cards are matched by the current card?
  cards_matched_by_current <- cards %>%
    filter(card == i) %>%
    pull(cards_won) %>%
    unlist()

  if(!is.null(cards_matched_by_current)){
    cards_so_far_df <- left_join(cards_so_far_df,
                                 tibble(card = cards_matched_by_current) %>%
                                   count(card), by = "card") %>%
      mutate(n = ifelse(is.na(n), 0, n),
             n = n * copies_of_current,
             copies = copies + n) %>%
      select(-n)
  }
}

# get total cards
cards_so_far_df$copies %>% sum()